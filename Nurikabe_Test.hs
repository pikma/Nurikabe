module Main where

import Nurikabe
import Test.HUnit
import qualified Data.Map as Map

simpleBoard = GameState {
  boardSize = 3, cellNumbers = Map.empty, cells = Map.empty }

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

testIsValid = [testIsValidInside, testIsValidOutside]
main = runTestTT $ TestList testIsValid
