module Nurikabe where

import Data.Maybe
import Data.List
import Control.Applicative
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

-- Returns True if the cell's state is known in the GameState. By default the
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

-- Return an element of the list which minimizes the function, or for which the
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
-- Game validity.
--
--  State is invalid if:
--   * a white area with a number is bigger than its number: done
--   * a white area has two numbers: done
--   * a non-black area with a number is smaller than its max number: done
--   * a connected non-white area, doesn't contain all blacks
--   * a square of black cells
--
-- Case that are okay, because the state is not complete yet:
--   * a white area has no number
--   * a non black area has no number
--   * a non black area has two numbers

type Area = [CellPosition]
type AreaStopFn = [CellPosition] -> Bool
type KeepCellFn = (Maybe CellState -> Bool)

-- Grows one step, returns Nothing if stopFn is true after growth. The returned
-- list is sorted and doesn't contain duplicates.
areaGrowOneStep :: AreaStopFn -> GameState -> KeepCellFn -> Area -> Maybe Area
areaGrowOneStep stopFn gs cellFn [] = error "List parameter shouldn't be empty"
areaGrowOneStep stopFn gs cellFn area =
  let newArea = area >>= (\p' -> p':(cellNeighbors gs p'))
      filtered = filter (\p' -> cellFn $ Map.lookup p' $ cells gs) newArea
      sorted = nub $ sort filtered in
    if stopFn newArea then Nothing else Just sorted

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
area stopFn gs cellFn p =
  areaGrowMax stopFn gs cellFn [p]

-- Return True if a connected white area contains more than 1 number, or more
-- cells than the island's number.
hasTwoNumbersInIsland :: GameState -> Bool
hasTwoNumbersInIsland gs =
  let hasTwoNumbers area =
        (>= 2) $ length $ filter (\p -> Map.member p $ cellNumbers gs) area
      areaStopFn x = let (Just x_num) = Map.lookup x (cellNumbers gs) in
          (||) <$> hasTwoNumbers <*> ((> x_num) . length)
  -- Take as parameter the list of positions that have a Number.
      aux numbers = case numbers of
        []     -> False
        (x:xs) ->
          let xArea' = area (areaStopFn x) gs (== Just White) x in
            case xArea' of
              Nothing    -> True
              -- No need to remove the area from xs, since it doesn't contain
              -- any Number cell.
              Just xArea -> aux xs in
     aux (Map.keys $ cellNumbers gs)

-- Return True if a connected area of non-black cells that contains a Number has
-- a size lower than this number.
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

hasNonConnectedRivers :: GameState -> Bool
hasNonConnectedRivers gs =
  -- Take as parameter the list of positions that are non-white.
  let aux nonWhite = case nonWhite of
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
  any (\p -> (== Nothing) $ Map.lookup p $ cells gs) (allPositions gs)

