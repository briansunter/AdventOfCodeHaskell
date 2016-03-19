module Day1.Day1 where

import           Data.List (elemIndex, foldl', scanl')
import           System.IO (readFile)

main = do
  contents <- readFile "input.txt"
  print $ santaFloor contents
  print $ santaFloorBasement contents

step :: Int -> Char -> Int
step fl x = if x == '(' then fl + 1 else fl - 1

santaFloor :: String -> Int
santaFloor = foldl' step 0

santaFloorBasement :: String -> Maybe Int
santaFloorBasement  s = elemIndex (-1) ( scanl step 0 s)
