{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec
import Day7.Day7
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.Map as M

main = hspec spec


spec :: Spec
spec = do
  describe "Day 7" $ do
    context "Gate Parsing" $ do
      it "should parse a not gate" $ do
        parseOnly unaryParser "NOT dq -> dr" `shouldBe` Right (Label "dr", (NotGate (Wire (Label "dq"))))
        parseOnly unaryParser "NOT 5 -> dr" `shouldBe` Right (Label "dr", (NotGate ((Signal 5))))
      it "should parse a wire"  $ do
        parseOnly wireParser "lx -> a" `shouldBe` Right(Label "a",(Wire(Label "lx")))
    context "Should run a Circuit" $ do
      let x = (Label("x"),Signal 123) :: Connection
      let y = (Label("y"),Signal 456) :: Connection
      let d = (Label("d"),AndGate (Wire (Label "x")) (Wire (Label "y"))) :: Connection
      let e = (Label("e"),OrGate (Wire (Label "x")) (Wire (Label "y"))) :: Connection
      let f = (Label("f"), LShiftGate 2 (Wire (Label "x")) ) :: Connection
      let g = (Label "g",RShiftGate 2 (Wire (Label "y")))
      let h = (Label "h", NotGate (Wire (Label "x")))
      let i = (Label "i",NotGate (Wire (Label "y")))
      let input = M.fromList [x,y,d,e,f,g,h,i]
      it "Run the examples" $ do
        runCircuit input (Wire (Label "x"))  `shouldBe` 123
        runCircuit input (Wire (Label "y"))  `shouldBe` 456
        runCircuit input (Wire (Label "i"))  `shouldBe` 65079
        runCircuit input (Wire (Label "h"))  `shouldBe` 65412
        runCircuit input (Wire (Label "g"))  `shouldBe` 114
        runCircuit input (Wire (Label "f"))  `shouldBe` 492
        runCircuit input (Wire (Label "e"))  `shouldBe` 507
        runCircuit input (Wire (Label "d"))  `shouldBe` 72
      it "should run a not gate" $ do
        runCircuit M.empty (NotGate (Signal 123)) `shouldBe` 65412
        let w = (Label("w"),Wire (Label "d")) :: Connection
        let t = (Label("t"),Wire (Label "w")) :: Connection
        runCircuit (M.fromList [x,y,d,w,t]) ((Wire (Label "t"))) `shouldBe` 72
