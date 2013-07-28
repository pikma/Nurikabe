module Main where

import Data.List
import Data.Maybe
import Nurikabe
import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set

board3 = emptyState 3

assertValid :: Bool -> CellPosition -> Assertion
assertValid b p = assertEqual
  ("Cell " ++ (show p) ++ " should " ++
    (if b then "" else "not ") ++ "be valid")
  b (isValidPos board3 p)

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

assertEqualAfter :: (Eq b, Show b) => (a -> b) -> String -> a -> a -> Assertion
assertEqualAfter f s a1 a2 = assertEqual s (f a1) (f a2)

testAllPositions = TestCase $ assertEqualAfter sort
  "allPositions returned incorrect results"
  [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]
  (allPositions board3)

oneCellKnown :: GameState
oneCellKnown = board3 `applyMove` ((1, 2), Black)

testIsKnownCell = TestCase $ do
  assertEqual
    "IsKnownCell"
    False (isKnownCell board3 (1,1))
  assertEqual
    "IsKnownCell" True (isKnownCell oneCellKnown (1, 2))
  assertEqual
    "IsKnownCell" False (isKnownCell oneCellKnown (2, 2))

testCellNeighbors = TestCase $ do
  assertEqualAfter sort "cellNeighbors"
    [(1, 2), (2, 1)] (cellNeighbors board3 (1, 1))

testNumUnknownNeighbors = TestCase $ do
  assertEqual "numUnknownNeighbors" 2 (numUnknownNeighbors board3 (1, 1))
  assertEqual "numUnknownNeighbors" 3 (numUnknownNeighbors board3 (2, 1))
  assertEqual "numUnknownNeighbors" 4 (numUnknownNeighbors board3 (2, 2))

(!!!) = applyMove

allCellsKnown :: GameState
allCellsKnown = board3
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

testArea = TestCase $ do
  let gs = ((emptyState 3) !!!
       ((2, 1), Black) !!! ((2, 3), Black) !!! ((3, 2), Black))
  assertEqual
    "Area"
    (Just $ Set.fromList [(1, 1), (1, 2), (2, 2), (1, 3)])
    (area (\_ -> False) gs (/= Just Black) (1, 1))
  assertEqual
    "Area"
    (Just $ Set.empty)
    (area (\_ -> False) gs (== Just White) (1, 1))
  assertEqual
    "Area"
    (Just $ Set.fromList (allPositions gs))
    (area (\_ -> False) gs (/= Just White) (1, 1))
  assertEqual
    "Area"
    Nothing
    (area ((> 4) . Set.size) gs (/= Just White) (1, 1))
  assertEqual
    "Area"
    (Just $ Set.fromList (allPositions gs))
    (area ((> 9) . Set.size) gs (/= Just White) (1, 1))

testTwoNumbersIsland = TestCase $ do
  let gs = (emptyState 3) !+!  ((2, 1), 3) !+! ((2, 3), 3)
  assertEqual
    "Island with two numbers"
    False
    (invalidConnectedWhite gs)
  assertEqual
    "Island with two numbers"
    True
    (invalidConnectedWhite $ gs !!! ((2, 2), White))
  assertEqual
    "Island with two numbers"
    True
    (invalidConnectedWhite $ gs !+! ((2, 2), 6))

testHasTooSmallIsland = TestCase $ do
  let assertEqual' b gs =
       assertEqual "Island too small" b $ hasTooSmallIsland gs
  assertEqual' False (emptyState 3)
  assertEqual' False ((emptyState 3) !+! ((1, 1), 4))
  assertEqual' False ((emptyState 3) !+! ((1, 1), 9))
  assertEqual' True ((emptyState 3) !+! ((1, 1), 10))
  let gs n = (emptyState 3) !+! ((2, 1), n) !!!
       ((1, 2), Black) !!!  ((2, 2), Black) !!! ((3, 2), Black)
  assertEqual' False $ gs 3
  assertEqual' True $ gs 4
  let gs' n = (emptyState 3) !+! ((2, 1), n) !!!
       ((1, 1), Black) !!! ((2, 2), Black) !!! ((3, 2), Black)
  mapM_ (\n -> assertEqual' False $ gs' n) [1..2]
  mapM_ (\n -> assertEqual' True $ gs' n) [3..9]

testHasNonConnectedRivers = TestCase $ do
  let assertEqual' b gs =
       assertEqual "Connected Rivers" b $ hasNonConnectedRivers gs
  assertEqual' False $ emptyState 3
  assertEqual' False $ (emptyState 3) !!! ((1, 2), White) !!! ((2, 2), White)
  assertEqual' False $ (emptyState 3)
      !!! ((1, 1), White) !!! ((1, 2), White) !!! ((1, 3), White)
  let testOneBoard gs = do
       assertEqual' False gs
       assertEqual' False $ gs !!! ((1, 1), Black)
       assertEqual' True $ gs !!! ((1, 1), Black) !!! ((1, 3), Black)
  testOneBoard $ (emptyState 3)
      !!! ((1, 2), White) !!! ((2, 2), White) !!! ((3, 2), White)
  testOneBoard $ (emptyState 3)
      !!! ((1, 2), White) !!! ((2, 2), White) !!! ((2, 3), White)
  testOneBoard $ (emptyState 3)
      !!! ((1, 2), White) !!! ((2, 2), White) !!! ((3, 3), White)

testHasBlackSquare = TestCase $ do
  let assertEqual' b gs =
       assertEqual "Connected Rivers" b $ hasBlackSquare gs
  assertEqual' False $ emptyState 3
  assertEqual' False $ emptyState 3 !!! ((2, 2), Black)
  assertEqual' False $ emptyState 3 !!! ((2, 2), Black) !!! ((2, 3), Black)
  assertEqual' False $ emptyState 3
      !!! ((2, 2), Black) !!! ((2, 3), Black) !!! ((3, 2), Black)
  assertEqual' False $ emptyState 3 !!! ((2, 2), Black)
      !!! ((2, 3), Black) !!! ((3, 2), Black) !!! ((3, 3), White)
  assertEqual' False $ emptyState 3 !!! ((2, 2), Black)
      !!! ((2, 3), Black) !!! ((3, 2), Black) !+! ((3, 3), 7)
  assertEqual' True $ emptyState 3 !!! ((2, 2), Black)
      !!! ((2, 3), Black) !!! ((3, 2), Black) !!! ((3, 3), Black)

testSplitOn = TestCase $ do
  assertEqual "splitOn1" ["foo", "bar", "baz"] $ splitOn ',' ",foo,bar,,baz,,,"
  assertEqual "splitOn2" ["foo", "bar", "baz"] $ splitOn ',' "foo,bar,baz"
  assertEqual "splitOn3" ["foobarbaz"]         $ splitOn ',' "foobarbaz"
  assertEqual "splitOn4" []                  $ splitOn ',' ""

tests = test [
  TestList [
    TestList [testIsValidOutside, testIsValidInside],
    testAllPositions,
    testIsKnownCell,
    testCellNeighbors,
    testNumUnknownNeighbors,
    testMostConstrainedCell,
    testArgMinWithBound
    ],
  testArea,
  testTwoNumbersIsland,
  testHasTooSmallIsland,
  testHasNonConnectedRivers,
  testSplitOn
  ]

main = runTestTT tests
