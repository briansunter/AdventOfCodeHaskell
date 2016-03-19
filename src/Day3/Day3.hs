module Day3.Day3 where
import           Data.List       (foldl', unzip)
import           Data.List.Split (chunksOf)
import qualified Data.Set        as S

import           System.IO       (readFile)

type Position = (Integer, Integer)

type HousesVisited = S.Set Position

type SantaState = (HousesVisited, Position)

data Direction = North | South | East | West

toDirection :: Char -> Direction
toDirection c = case c of
  '^' -> North
  'v' -> South
  '>' -> East
  '<' -> West

initialState :: SantaState
initialState = (S.singleton (0,0), (0,0))

nextPosition :: Position -> Direction-> Position
nextPosition (x,y) d = case d of
  North -> (x, y+1)
  South -> (x, y-1)
  East  -> (x+1, y)
  West  -> (x-1, y)

changePosition :: SantaState -> Direction -> SantaState
changePosition (visited,pos) dir = (S.insert nextPos visited, nextPos) where
  nextPos = nextPosition pos dir

housesVisited :: String -> SantaState
housesVisited = foldl changePosition initialState . map toDirection

main = do
  input <- readFile "input.txt"
  let (houses,end) = housesVisited input
  print $ S.size houses

  let (santaHouses,end) = housesVisited $ santaList input
  let (robotHouses,end) = housesVisited $ robotList input
  print $ S.size $ S.union santaHouses robotHouses


santaList :: [a] -> [a]
santaList (a:b:rest) = a:santaList rest
santaList (a:_) = [a]
santaList _ = []

robotList:: [a] -> [a]
robotList (a:b:rest) = b:robotList rest
robotList  _ = []
