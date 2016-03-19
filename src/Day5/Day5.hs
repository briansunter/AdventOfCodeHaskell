{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day5.Day5 where
import           Text.Regex.PCRE.Heavy

atLeastThreeVowels ::  String -> Bool
atLeastThreeVowels x = x =~ [re|(.*[aeiou]){3}|]

twoConsecutiveLetters :: String -> Bool
twoConsecutiveLetters x = x =~ [re|(.)\1|]

doesNotContainLetters :: String -> Bool
doesNotContainLetters x = not $ x =~ [re|(ab|cd|pq|xy)|]

twoLettersAppearTwice :: String -> Bool
twoLettersAppearTwice x = x =~ [re|\b\w*?(\w{2})\w*?\1\w*?\b|]

letterSandwich :: String -> Bool
letterSandwich x = x=~ [re|(.).\1|]

passesTests :: [String -> Bool] -> String -> Bool
passesTests ts i = all (\t -> t i) ts

part1Test :: String -> Bool
part1Test = passesTests [atLeastThreeVowels, twoConsecutiveLetters, doesNotContainLetters]

part2Test :: String -> Bool
part2Test = passesTests [twoLettersAppearTwice, letterSandwich]

main = do
  file <- readFile "src/Day5/input.txt"
  print $ length $ filter part1Test (lines file)

  print $ length $ filter part2Test (lines file)
