{-# LANGUAGE OverloadedStrings #-}
module Day6.Parser where
import           Control.Applicative              (many, (<|>))
import           Data.Attoparsec.ByteString.Char8
import           Day6.Day6

lightActionParser :: Parser LightAction
lightActionParser = (string "turn on"  >> return TurnOn)
                <|> (string "turn off" >> return TurnOff)
                <|> (string "toggle"   >> return Toggle)

pointParser :: Parser Point
pointParser = do
  x <- decimal
  char ','
  y <- decimal
  return (x,y)

pointParser2 :: Parser Point
pointParser2 = (,) <$> (decimal <* char ',')
                   <*> decimal

parseInstruction2 :: Parser Instruction
parseInstruction2 = Instruction <$> (lightActionParser <* space)
                                <*> (pointParser <* string " through ")
                                <*> pointParser

parseInstruction :: Parser Instruction
parseInstruction = do
  a <- lightActionParser
  space
  p1 <- pointParser
  string " through "
  p2 <- pointParser
  return $ Instruction a p1 p2

parseAllInstructions :: Parser [Instruction]
parseAllInstructions = many $ parseInstruction <* endOfLine
