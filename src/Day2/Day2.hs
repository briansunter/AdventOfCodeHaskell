{-# LANGUAGE OverloadedStrings #-}
module Day2.Day2 where
import           Data.List      (sort,foldl')
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           Data.Text.Read

type Dimensions = (Integer, Integer, Integer)

main = do
    file <- T.readFile "input.txt"
    let a = map (T.splitOn "x") $ T.lines file
    let b = map tuple a
    print $ totalPaperAmount b
    print $ totalRibbonLength b


tuple :: [T.Text] -> Dimensions
tuple [l,w,h] = case (decimal l, decimal w,decimal h) of
  (Right (l,_), Right (w,_) , Right (h,_)) -> (l,w,h)

paperAmount :: Dimensions -> Integer
paperAmount d = boxSurfaceArea d + smallestSidesArea d

boxSurfaceArea :: Dimensions -> Integer
boxSurfaceArea (l,w,h) = 2*l*w + 2*w*h + 2*h*l

smallestSides :: Dimensions -> (Integer,Integer)
smallestSides (l,w,h) = arrayToTuple $ take 2 $ sort [l,w,h]

smallestSidesArea :: Dimensions -> Integer
smallestSidesArea d = let (f,s) = smallestSides d in f*s

totalPaperAmount :: [Dimensions] -> Integer
totalPaperAmount = foldl' (\acc x -> acc + paperAmount x) 0

smallestSidesPerimeter :: Dimensions -> Integer
smallestSidesPerimeter d = let (f,s) = smallestSides d in 2*f + 2*s

boxVolume :: Dimensions -> Integer
boxVolume (l,w,h) = l*w*h

ribbonLength:: Dimensions -> Integer
ribbonLength d = smallestSidesPerimeter d + boxVolume d

totalRibbonLength :: [Dimensions] -> Integer
totalRibbonLength = foldl' (\acc x -> acc + ribbonLength x) 0

arrayToTuple :: [a] -> (a,a)
arrayToTuple [f,s] = (f,s)
