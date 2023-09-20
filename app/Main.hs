module Main (main) where

import Lib
import System.Random

main :: IO ()
main = print (show result {boards = take 10 (boards result)})
  where
    result = simulate (Game [initialBoard] (mkStdGen 5) Red 0 (Movement 0 0) 0)
