module Day6.Main where
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  as BS
import qualified Data.Vector.Unboxed              as V
import           Day6.Day6
import           Day6.Parser                      (parseAllInstructions)

main = do
    input <- BS.readFile "./src/Day6/input.txt"
    let (Right instructions) = parseOnly parseAllInstructions input
    let emptyYard = V.replicate (1000 * 1000) False
    yard <- updateWithInstructions emptyYard instructions

    print "Part 1"
    print $ V.length $ V.filter id yard

    let emptyYard2 =  V.replicate (1000*1000) (0 :: Int)
    grid2 <- updateWithInstructions emptyYard2 instructions
    print "Part 2"
    print $ V.sum grid2
