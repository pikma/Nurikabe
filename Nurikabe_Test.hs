module Main where

import Data.List
import Nurikabe
import Test.HUnit
import qualified Data.Map as Map

simpleBoard = emptyState 3

assertValid :: Bool -> CellPosition -> Assertion
assertValid b p = assertEqual
  ("Cell " ++ (show p) ++ " should " ++
    (if b then "" else "not ") ++ "be valid")
  b (isValidPos simpleBoard p)

testIsValidOutside = TestCase $ do
  assertValid False (0, 1)
  assertValid False (1, 0)
  assertValid False (0, 0)

  assertValid False (3, 4)
  assertValid False (4, 3)
  assertValid False (4, 4)

  assertValid False (1, 4)
  assertValid False (0, 3)
  assertValid False (0, 4)

  assertValid False (4, 1)
  assertValid False (3, 0)
  assertValid False (4, 0)

testIsValidInside = TestCase $ do
  assertValid True (1, 1)
  assertValid True (3, 1)
  assertValid True (1, 3)
  assertValid True (3, 3)

assertSortedEqual :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSortedEqual s l1 l2 = assertEqual s (sort l1) (sort l2)

testAllPositions = TestCase $ assertSortedEqual
  "allPositions returned incorrect results"
  [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]
  (allPositions simpleBoard)

oneCellKnown :: GameState
oneCellKnown = simpleBoard `applyMove` ((1, 2), Black)

testIsKnownCell = TestCase $ do
  assertEqual
    "IsKnownCell"
    False (isKnownCell simpleBoard (1,1))
  assertEqual
    "IsKnownCell" True (isKnownCell oneCellKnown (1, 2))
  assertEqual
    "IsKnownCell" False (isKnownCell oneCellKnown (2, 2))

testCellNeighbors = TestCase $ do
  assertSortedEqual "cellNeighbors"
    [(1, 2), (2, 1)] (cellNeighbors simpleBoard (1, 1))

testNumUnknownNeighbors = TestCase $ do
  assertEqual "numUnknownNeighbors" 2 (numUnknownNeighbors simpleBoard (1, 1))
  assertEqual "numUnknownNeighbors" 3 (numUnknownNeighbors simpleBoard (2, 1))
  assertEqual "numUnknownNeighbors" 4 (numUnknownNeighbors simpleBoard (2, 2))

(!!!) = applyMove

allCellsKnown :: GameState
allCellsKnown = simpleBoard
  !!! ((1, 1), Black) !!! ((1, 2), Black) !!! ((1, 3), Black)
  !!! ((2, 1), Black) !!! ((2, 2), Black) !!! ((2, 3), Black)
  !!! ((3, 1), Black) !!! ((3, 2), Black) !!! ((3, 3), Black)

testMostConstrainedCell = TestCase $ do
  assertEqual "mostConstrainedCell"
    (Just (1, 1)) (mostConstrainedCell oneCellKnown)
  let cell23Known = (emptyState 4) !!! ((3, 4), White)
  assertEqual "mostConstrainedCell"
    (Just (4, 4)) (mostConstrainedCell cell23Known)
  let allNeighborsKnown = (emptyState 4)
        !!! ((4, 4), White) !!! ((2, 4), White) !!! ((3, 3), White)
  assertEqual "mostConstrainedCell"
    (Just (3, 4)) (mostConstrainedCell allNeighborsKnown)
  assertEqual "mostConstrainedCell"
    (Nothing) (mostConstrainedCell allCellsKnown)

testArgMinWithBound = TestCase $ do
  let aux n b l = do
       assertEqual
         ("argMinWithBound " ++ show b ++ " " ++ show (take 10 l))
         n (argMinWithBound (id :: Int -> Int) b l)
       assertEqual
         ("argMinWithBound (negate) " ++ show b ++ " " ++
             show (take 10 $ map negate l))
         (-n) (argMinWithBound negate b (map negate l))
  aux 2 3 ([5, 2] ++ [6 ..])
  aux 3 3 ([5, 3] ++ [6 ..])
  aux 4 3 ([5, 4] ++ enumFromTo 6 19)

(!+!) = addNumber

testParseState = TestCase $ assertEqual
  "ParseState"
  (Right $ (emptyState 5)
    !+! ((2, 1), 3) !+! ((2, 3), 3) !+!  ((5, 1), 2) !+! ((5, 3), 4))
  (parseState
   "5\n\n-----\n # a comment\nt 3\n\n\n2 4 #some comment\n-----")

tests = [
  TestList [
    TestList [testIsValidOutside, testIsValidInside],
    testAllPositions,
    testIsKnownCell,
    testCellNeighbors,
    testNumUnknownNeighbors,
    testMostConstrainedCell,
    testArgMinWithBound
    ],
  testParseState
  ]

main = runTestTT $ TestList tests
