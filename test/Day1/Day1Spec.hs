module Day1Spec where
import           Day1.Day1  (santaFloor,santaFloorBasement)
import           Test.Hspec
import           System.Exit     (exitFailure)
import           Test.QuickCheck
import Data.Maybe(isJust)
import Safe (headMay)

runSpec = hspec spec

spec :: Spec
spec = do
  describe "Day7 Part 1" $ do
    it "should run the examples" $ do
      santaFloor "(())" `shouldBe` 0
      santaFloor "(((" `shouldBe` 3
      santaFloor "(()(()(" `shouldBe` 3
      santaFloor "))(((((" `shouldBe` 3
      santaFloor "())" `shouldBe` -1
      santaFloor "))(" `shouldBe` -1
      santaFloor ")))" `shouldBe` -3
      santaFloor ")())())" `shouldBe` -3

    it "should find the correct end floor for any input" $ do
      property testCorrectEndingFloor

  describe "Day 7 Part 2" $ do
    it "runs the examples" $ do
      santaFloorBasement ")" `shouldBe` Just 1
      santaFloorBasement "()())" `shouldBe` Just 5

    it "identifies if he never enters the basement" $ do
      santaFloorBasement "((" `shouldBe` Nothing

    it "always enters the basement if there are more ) than (" $ do
      property testDoesEnterBasement

testCorrectEndingFloor = forAll testEndingFloorInput (\(ins, expected) -> santaFloor ins == expected )

testDoesEnterBasement = forAll testBasementInput $ isJust . santaFloorBasement

testEndingFloorInput :: Gen (String,Int)
testEndingFloorInput = do
  closed  <- listOf $ elements ")"
  open <- listOf $ elements "("
  shuffled <- shuffle $ open ++ closed
  return (shuffled, length open - length closed)

testBasementInput  :: Gen String
testBasementInput = do
  closed  <- listOf1 $ elements ")"
  openSized <- choose(0, length closed - 1)
  open <- vectorOf openSized $ elements "("
  shuffle $ open ++ closed
