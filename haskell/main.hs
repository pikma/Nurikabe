module Main where

import Data.List
import Nurikabe

main = do
  boardStr <- getContents
  putStr "Initial board:\n"
  putStr boardStr
  case parseState boardStr of
     Left error -> putStr (error ++ "\n")
     Right board -> case solve board of
       Nothing -> putStr "No solution found."
       Just solution -> do
         putStr "\nSolution found:\n"
         putStr $ printState solution

