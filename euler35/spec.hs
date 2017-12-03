-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler35

main :: IO ()
main = hspec $ do
  describe "Euler34" $ do
    describe "digits" $ do
      it "builds digit list for simple cases" $ do
        digits 1 `shouldBe` [1]
        digits 12 `shouldBe` [1, 2]
        digits 123 `shouldBe` [1, 2, 3]

    describe "combine" $ do
      it "creates number back from list of digits" $ do
        combine [1] `shouldBe` 1
        combine [9, 0] `shouldBe` 90

    describe "listRotations" $ do
      it "creates all shifts of digits in the list" $ do
        listRotations [1] `shouldBe` [[1]]
        listRotations [9,0] `shouldBe` [[9,0], [0,9]]
        listRotations [1,2,3] `shouldBe` [[1,2,3], [2,3,1], [3,1,2]]

    describe "rotations" $ do
      it "removes all duplicates" $ do
        rotations 90 `shouldBe` [90, 9]
        rotations 111 `shouldBe` [111]
        rotations 9779 `shouldBe` [9779, 7799, 7997, 9977]

    describe "allAre" $ do
      it "work correctly with numbers" $ do
        allAre (>10) [11, 12] `shouldBe` True
        allAre (>10) [11, 2] `shouldBe` False
        allAre (>10) [2, 2] `shouldBe` False

    describe "circularPrime" $ do
      it "work correctly with given numbers" $ do
        circularPrime 1 `shouldBe` False
        circularPrime 2 `shouldBe` True
        circularPrime 31 `shouldBe` True
        circularPrime 19 `shouldBe` False



