{-# LANGUAGE OverloadedStrings #-}
module Day7.Parser where
import           Control.Applicative              (many, (<|>))
import           Control.Monad                    (liftM)
import           Data.Attoparsec.Text
import           Day7.Day7
import Data.Text (Text)

type Connection = (Label,Circuit)

binaryGate :: Text -> Circuit -> Circuit -> Circuit
binaryGate s i1 i2 = case s of
  "AND" -> AndGate i1 i2
  "OR"  -> OrGate i1 i2

shiftGate:: Text -> Int -> Circuit -> Circuit
shiftGate s amount i = case s of
  "LSHIFT"  -> LShiftGate amount i
  "RSHIFT"  -> RShiftGate amount i

parseGateInput :: Parser Circuit
parseGateInput = liftM Signal decimal
             <|> liftM (Wire . Label) takeText

labelSeparator :: Parser Char
labelSeparator = do
  space
  string "->"
  space

unaryParser :: Parser Connection
unaryParser = do
  string "NOT"
  space
  input <- parseGateInput
  labelSeparator
  identifier <- takeText
  return (Label identifier, NotGate input)

binaryParser :: Parser Connection
binaryParser = do
  input1 <- parseGateInput
  space
  binaryGateType <- takeText
  space
  input2 <- parseGateInput
  labelSeparator
  identifier <- takeText
  return (Label identifier, binaryGate binaryGateType input1 input2)

shiftParser :: Parser Connection
shiftParser = do
  input <- parseGateInput
  space
  shiftGateType <- takeText
  space
  shiftAmount <- decimal
  labelSeparator
  identifier <- takeText
  return (Label identifier, shiftGate shiftGateType shiftAmount input)

signalParser :: Parser Connection
signalParser = do
  input <- decimal
  labelSeparator
  identifier <- takeText
  return (Label identifier, Signal input)

wireParser :: Parser Connection
wireParser = do
  input <- takeText
  labelSeparator
  identifier <- takeText
  return (Label identifier, Wire (Label input))

parseCircuit :: Parser Connection
parseCircuit = signalParser
           <|> unaryParser
           <|> shiftParser
           <|> binaryParser
           <|> wireParser

parseAllCircuits :: Parser [(Label,Circuit)]
parseAllCircuits = many $ parseCircuit <* endOfLine
