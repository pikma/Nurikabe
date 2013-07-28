module Nurikabe where

import Control.Applicative
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

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

allPositions :: GameState -> [CellPosition]
allPositions gs =
  let indices = [1 .. (boardSize gs)] in
    [(x, y) | x <- indices, y <- indices]

--------------------------------------------------------------------------------
-- Finding which cell to play next.

-- Returns True if the cell's state is known in the GameState. By default the
-- state of a cell which has a Number is unknown, assigning its state to White
-- must be done manually.
isKnownCell :: GameState -> CellPosition -> Bool
isKnownCell gs pos = Map.member pos (cells gs)

numUnknownNeighbors :: GameState -> CellPosition -> Int
numUnknownNeighbors gs p =
  length $ filter (not . (isKnownCell gs)) (cellNeighbors gs p)

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

type Area = Set.Set CellPosition
type AreaStopFn = Area -> Bool
type KeepCellFn = (Maybe CellState -> Bool)

-- Grows one step, returns Nothing if stopFn is true after growth. The returned
-- list is sorted and doesn't contain duplicates.
areaGrowOneStep :: AreaStopFn -> GameState -> KeepCellFn -> Area -> Maybe Area
areaGrowOneStep stopFn gs cellFn area =
  let addNeighbors set p = Set.union set (Set.fromList $ cellNeighbors gs p) in
  let newArea = Set.foldl addNeighbors area area in
  let filtered =
       Set.filter (\p' -> cellFn $ Map.lookup p' $ cells gs) newArea in
         if stopFn filtered then Nothing else Just filtered

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
area stopFn gs cellFn p = areaGrowMax stopFn gs cellFn (Set.singleton p)

-- Returns True if either:
--   * a connected white area has two numbers;
--   * a connected white area with a number n has more than n cells;
invalidConnectedWhite :: GameState -> Bool
invalidConnectedWhite gs =
  let hasTwoNumbers area =
        (>= 2) $ Set.size $ Set.filter (\p -> Map.member p $ cellNumbers gs) area
      -- Take as parameter the list of positions that have a Number.
      aux numbers = case numbers of
        []     -> False
        (x:xs) ->
          let (Just x_num) = Map.lookup x (cellNumbers gs)
              xArea' = area
                  ((||) <$> hasTwoNumbers <*> ((> x_num) . Set.size))
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
          let Just xArea = area (\_ -> False) gs (/= Just Black) x in
          let (Just islandNumber) = Map.lookup x (cellNumbers gs) in
               if Set.size xArea < islandNumber then True
               else aux (xs \\ Set.toList xArea) in
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
            any (\p -> Set.notMember p xArea) xs in
    aux $ Map.keys $ Map.filter (== Black) $ cells gs

hasBlackSquare :: GameState -> Bool
hasBlackSquare gs =
  let isBlack p = (== Just Black) $ Map.lookup p $ cells gs
  -- The position (x, y) must be black itself.
      isBottomLeftCornerOfBlackSquare (x, y) =
        isBlack (x+1, y) && isBlack (x, y+1) && isBlack (x+1, y+1)
      blacks = Map.keys $ Map.filter (== Black) $ cells gs in
        any isBottomLeftCornerOfBlackSquare blacks

isLegal :: GameState -> Bool
isLegal gs =
  let illegalFns = [invalidConnectedWhite,
                     hasTooSmallIsland,
                     hasNonConnectedRivers,
                     hasBlackSquare] in
  not $ any (\f -> f gs) illegalFns

isFinished :: GameState -> Bool
isFinished gs =
  not $ any (\p -> (Map.lookup p $ cells gs) == Nothing) (allPositions gs)

--------------------------------------------------------------------------------
-- Game parsing and outputting

foldlMWithIndex :: (Monad m) => (Int -> a -> b -> m b) -> b -> [a] -> m b
foldlMWithIndex f init l =
  let g bb (i, aa) = f i aa bb in Foldable.foldlM g init (zip [1..] l)

addNumber :: GameState -> (CellPosition, Int) -> GameState
addNumber gs (p, n) =
  GameState {
    boardSize = boardSize gs,
    cellNumbers = Map.insert p n (cellNumbers gs),
    cells = Map.insert p White (cells gs) }

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, _)] -> Just x
    _         -> Nothing

-- Ignores empty tokens.
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn d l =
  let splitOn' result tmp remaining = case remaining of
         [] -> reverse (if null tmp then result else (reverse tmp) : result)
         (x:xs) -> if x == d
           then if null tmp then splitOn' result [] xs
                       else splitOn' ((reverse tmp) : result) [] xs
           else splitOn' result (x:tmp) xs in
  splitOn' [] [] l

parseState :: String -> Either String GameState
parseState contents =
  let allLines = lines contents in
  let nbLines = length allLines in
  if even nbLines
    then Left ("Error: Even number of lines (" ++ show nbLines ++ ")")
    else
  let n = div (nbLines - 1)  2 in
  let goodLines = filter (/= (lineDelimiter n)) allLines in
  let processToken i j token gs = case token of
       "   " -> Right gs
       " # " -> Right $ applyMove gs ((i, j), Black)
       _     -> case maybeRead token  of
                  Just n -> Right $ addNumber gs ((i, j), n)
                  Nothing -> Left $ show (i, j) ++ ": Invalid token '" ++ token ++ "'" in
  let processLine i line gs =
       let tokens = splitOn '|' line in
       foldlMWithIndex (processToken i) gs tokens in
  foldlMWithIndex processLine (emptyState n) goodLines

pintersperse :: a -> [a] -> [a]
pintersperse d l = d : ((intersperse d l) ++ [d])

lineDelimiter :: Int -> String
lineDelimiter n = replicate (4*n + 1) '-'

printState :: GameState -> String
printState gs =
  let n = boardSize gs in
  let printPos p = case Map.lookup p $ cellNumbers gs of
        Just n -> " " ++ show n ++ (if (n < 10) then " " else "")
        Nothing -> case Map.lookup p $ cells gs of
           Just White -> "   "
           Just Black -> " # "
           Nothing    -> " ? " in
  let printLine i =
        let positions = map ((,) i) [1 .. n] in
              concat $ pintersperse "|" $ map printPos positions in
  let lines = map printLine [1 .. n] in
    concat $ pintersperse "\n" $ pintersperse (lineDelimiter n) lines

--------------------------------------------------------------------------------
-- Finding a solution.

otherMove :: Move -> Move
otherMove (p, Black) = (p, White)
otherMove (p, White) = (p, Black)

solve :: GameState -> Maybe GameState
solve gs =
  let candidateCells = filter (not . (isKnownCell gs)) (allPositions gs) in
  --  trace ("Reset. Num candidates : " ++ (show $ length candidateCells)) $
  if null candidateCells then Just gs else
  case tryCandidates gs candidateCells of
        Nothing -> Nothing
        Just gs' -> if gs == gs' then solveWithGuess gs' else solve gs'

unknownNeighbors :: GameState -> [CellPosition] -> [CellPosition]
unknownNeighbors gs l =
  let aux l set = case l of
       [] -> Set.toList set
       (p:xs) ->
         let neighbors = filter (not . (isKnownCell gs)) (cellNeighbors gs p) in
               aux xs (Set.union set $ Set.fromList neighbors) in
       aux l Set.empty

tryCandidates :: GameState -> [CellPosition] -> Maybe GameState
tryCandidates gs candidates =
   --  trace ("Num candidates : " ++ (show $ length candidates)) $
   if not (isLegal gs) then Nothing
   else if null candidates then solve gs
   else
     let applyMoveIfForced (gs, appliedPos) p =
          let isWhiteLegal = isLegal $ applyMove gs (p, White) in
          let isBlackLegal = isLegal $ applyMove gs (p, Black) in
               if (isWhiteLegal && not isBlackLegal)
                 then Just (applyMove gs (p, White), p:appliedPos)
               else if (isBlackLegal && not isWhiteLegal)
                 then Just (applyMove gs (p, Black), p:appliedPos)
               else if (not isBlackLegal && not isWhiteLegal)
                 then Nothing
               else Just (gs, appliedPos) in
     case Foldable.foldlM applyMoveIfForced (gs, []) candidates of
           Nothing -> Nothing
           Just (gs', applied) ->
             if null applied then Just gs'
             else tryCandidates gs' $ unknownNeighbors gs' applied

solveWithGuess :: GameState -> Maybe GameState
solveWithGuess gs =
  --  trace "Guess." $
  case mostConstrainedCell gs of
     Nothing -> Nothing
     Just p -> case solve $ applyMove gs (p, White) of
         Nothing -> solve $ applyMove gs (p, Black)
         Just solution -> Just solution
