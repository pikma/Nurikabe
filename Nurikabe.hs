module Nurikabe where

import Control.Applicative
import qualified Data.Foldable as Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data CellState = Black | White deriving (Eq, Show)

-- Indices start at 1.
type CellPosition = (Int, Int)

type Move = (CellPosition, CellState)

data GameState = GameState {
  boardSize :: Int,
  cellNumbers :: Map.Map CellPosition Int,
  cells :: Map.Map CellPosition CellState
} deriving (Show, Eq)

emptyState :: Int -> GameState
emptyState n = GameState {
  boardSize = n, cellNumbers = Map.empty, cells = Map.empty }

applyMove :: GameState -> Move -> GameState
applyMove gs (p, s) = GameState {
  boardSize = boardSize gs,
  cellNumbers = cellNumbers gs,
  cells = Map.insert p s (cells gs)}

-- Diagonal are not neighbors
cellNeighbors :: GameState -> CellPosition -> [CellPosition]
cellNeighbors gs (x, y) =
  filter (isValidPos gs)
    [(x + fst d, y + snd d) | d <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]

isValidPos :: GameState -> CellPosition -> Bool
isValidPos gs (x, y) = (x >= 1 && x <= boardSize gs &&
                        y >= 1 && y <= boardSize gs)

-- Returnss True if the cell's state is known in the GameState. By default the
-- state of a cell which has a Number is unknown, assigning its state to White
-- must be done manually.
isKnownCell :: GameState -> CellPosition -> Bool
isKnownCell gs pos = Map.member pos (cells gs)

numUnknownNeighbors :: GameState -> CellPosition -> Int
numUnknownNeighbors gs p =
  length $ filter (not . (isKnownCell gs)) (cellNeighbors gs p)

allPositions :: GameState -> [CellPosition]
allPositions gs =
  let indices = [1 .. (boardSize gs)] in
    [(x, y) | x <- indices, y <- indices]

-- Returns Nothing if all cells are known, otherwise returns the cell with the
-- least number of unknown neighbors.
mostConstrainedCell :: GameState -> Maybe CellPosition
mostConstrainedCell gs =
  let unknownCells = filter (not . (isKnownCell gs)) (allPositions gs) in
  case unknownCells of
    []        -> Nothing
    otherwise -> Just $ argMinWithBound (numUnknownNeighbors gs) 0 unknownCells

-- Returns an element of the list which minimizes the function, or for which the
-- value of the function is no greater to the bound given in second argument.
argMinWithBound :: (Ord b, Bounded b) => (a -> b) -> b -> [a] -> a
argMinWithBound f bound [] = error "List parameter shouldn't be empty"
argMinWithBound f bound l@(x:xs) =
  let aux ll current_arg_min current_min =
        case ll of
          []     -> current_arg_min
          (x:xs) -> let val = f x in
                      if val <= bound then x
                      else if val < current_min then aux xs x (val)
                      else aux xs current_arg_min current_min in
     aux l x maxBound

--------------------------------------------------------------------------------
-- Game parsing and outputting
foldrMWithIndex :: (Monad m) => (Int -> a -> b -> m b) -> b -> [a] -> m b
foldrMWithIndex f init l =
  let g (i, aa) = f i aa in Foldable.foldrM g init (zip [1..] l)

addNumber :: GameState -> (CellPosition, Int) -> GameState
addNumber gs (p, n) =
  GameState {
    boardSize = boardSize gs,
    cellNumbers = Map.insert p n (cellNumbers gs),
    cells = Map.insert p White (cells gs) }

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

parseState :: String -> Either String GameState
parseState contents =
  let first:rest = map (takeWhile (/= '#')) $ lines contents
      n = read first :: Int
      board_lines = take n $ tail $ dropWhile (/= (replicate n '-')) rest
      aux i j c gs =
        if c == ' ' then Right gs else
        case maybeRead [c] of
          Nothing -> Left ("Can't parse number '" ++ c:"'")
          Just number -> let p = (i,j) in Right $ addNumber gs (p, number)
      aux' i line gs = foldrMWithIndex (aux i) gs line in
  foldrMWithIndex aux' (emptyState n) board_lines

--------------------------------------------------------------------------------
-- Game validity.
--
--  As we progress in the game, a cell can either be White, Black, or Unknown. A
--  cell with a Number is considered White. We define a set of properties such
--  that:
--    The final state reached after moves m1, ... m_n is invalid iff. there
--    exists a move m_i after which one of those properties is true.
--
--  Here are those properties:
--   * a connected white area has two numbers;
--   * a connected white area with a number n has more than n cells;
--   * a connected non-black area with a number n has less than n cells;
--   * there is a square of 4 black cells;
--   * there are more than one connected non-white areas that contains a black.

type Area = [CellPosition]
type AreaStopFn = [CellPosition] -> Bool
type KeepCellFn = (Maybe CellState -> Bool)

-- Grows one step, returns Nothing if stopFn is true after growth. The returned
-- list is sorted and doesn't contain duplicates.
areaGrowOneStep :: AreaStopFn -> GameState -> KeepCellFn -> Area -> Maybe Area
areaGrowOneStep stopFn gs cellFn [] = Just []
areaGrowOneStep stopFn gs cellFn area =
  let newArea = area >>= (\p' -> p':(cellNeighbors gs p'))
      filtered = filter (\p' -> cellFn $ Map.lookup p' $ cells gs) newArea
      sorted = nub $ sort filtered in
    if stopFn sorted then Nothing else Just sorted

-- Grows until either stopFn is true or growing doesn't change the area
-- anymore. The returned list is sorted and doesn't contain duplicates. Returns
-- Nothing is the condition stopFn is true.
areaGrowMax :: AreaStopFn -> GameState -> KeepCellFn -> Area -> Maybe Area
areaGrowMax stopFn gs cellFn area =
  let newArea = areaGrowOneStep stopFn gs cellFn area in
    case newArea of
      Nothing -> Nothing
      Just newArea' ->
        if newArea' == area then Just newArea'
        else areaGrowMax stopFn gs cellFn newArea'

-- Iteratively grows an area of cells that verify the KeepCellFn predicate from
-- the given CellPosition. At each step, if the current area verifies the
-- AreaStopFn predicate, stops and returns Nothing.
area :: AreaStopFn -> GameState -> KeepCellFn -> CellPosition -> Maybe Area
area stopFn gs cellFn p = areaGrowMax stopFn gs cellFn [p]

-- Returns True if either:
--   * a connected white area has two numbers;
--   * a connected white area with a number n has more than n cells;
invalidConnectedWhite :: GameState -> Bool
invalidConnectedWhite gs =
  let hasTwoNumbers area =
        (>= 2) $ length $ filter (\p -> Map.member p $ cellNumbers gs) area
      -- Take as parameter the list of positions that have a Number.
      aux numbers = case numbers of
        []     -> False
        (x:xs) ->
          let (Just x_num) = Map.lookup x (cellNumbers gs)
              xArea' = area
                  ((||) <$> hasTwoNumbers <*> ((> x_num) . length))
                  gs (== Just White) x in
            case xArea' of
              Nothing    -> True
              Just xArea -> aux xs in
  aux (Map.keys $ cellNumbers gs)

-- Returns True if a connected non-black area with a number n has less than n
-- cells.
hasTooSmallIsland :: GameState -> Bool
hasTooSmallIsland gs =
  -- Take as parameter the list of positions that have a Number.
  let aux numbers = case numbers of
        []     -> False
        (x:xs) ->
          let xArea' = area (\_ -> False) gs (/= Just Black) x in
            case xArea' of
              Nothing -> True
              Just xArea ->
                let (Just islandNumber) = Map.lookup x (cellNumbers gs) in
                     if length xArea < islandNumber then True
                     else aux (xs \\ xArea) in
      aux (Map.keys $ cellNumbers gs)

-- Returns True if there is more than one connected non-white area that contains
-- a black (if it doesn't contain a black, it's okay as it could become all
-- white).
hasNonConnectedRivers :: GameState -> Bool
hasNonConnectedRivers gs =
  -- Take as parameter the list of positions that are non-white.
  let aux blacks = case blacks of
        []     -> False
        (x:xs) ->
          let (Just xArea) = area (\_ -> False) gs (/= Just White) x in
            any (\p -> notElem p xArea) xs in
    aux $ Map.keys $ Map.filter (== Black) $ cells gs

hasBlackSquare :: GameState -> Bool
hasBlackSquare gs =
  let isBlack p = (== Just Black) $ Map.lookup p $ cells gs
  -- The position (x, y) must be black itself.
      isBottomLeftCornerOfBlackSquare (x, y) =
        isBlack (x+1, y) && isBlack (x, y+1) && isBlack (x+1, y+1)
      blacks = Map.keys $ Map.filter (== Black) $ cells gs in
        any isBottomLeftCornerOfBlackSquare blacks

isFinished :: GameState -> Bool
isFinished gs =
  any (\p -> (Map.lookup p $ cells gs) == Nothing) (allPositions gs)

