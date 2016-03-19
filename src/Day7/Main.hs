module Day7.Main where
import           Control.Monad.Memo               (startEvalMemo)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS
import qualified Data.Map                         as M
import           Day7.Day7
import           Day7.Parser

run = do
  input <- BS.readFile "./src/Day7/input.txt"
  let (Right instructions) = parseOnly parseAllCircuits input
  let circuitBox = M.fromList instructions
  let part1 = startEvalMemo $ runCircuit circuitBox (Wire (Label "a"))
  print part1

  let part2CircuitBox = M.insert (Label "b") (Signal part1) circuitBox
  let part2 = startEvalMemo $ runCircuit part2CircuitBox (Wire (Label "a"))
  print part2
