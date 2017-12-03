-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler34

main :: IO ()
main = hspec $ do
  describe "Euler34" $ do
    describe "factorial" $ do
      it "works for simple cases" $ do
        factorial 0 `shouldBe` 1
        factorial 1 `shouldBe` 1
        factorial 2 `shouldBe` 2
        factorial 3 `shouldBe` 6
        factorial 4 `shouldBe` 24
        factorial 5 `shouldBe` 120

    describe "digits" $ do
      it "builds digit list fir simple cases" $ do
        digits 1 `shouldBe` [1]
        digits 12 `shouldBe` [1, 2]
        digits 123 `shouldBe` [1, 2, 3]

    describe "digiFactoSum" $ do
      it "works for simple cases" $ do
        digiFactoSum 1 `shouldBe` 1
        digiFactoSum 12 `shouldBe` 3
        digiFactoSum 123 `shouldBe` 9

    describe "curious" $ do
      it "works in known cases" $ do
        curious 145 `shouldBe` True
        curious 1 `shouldBe` True
        curious 2 `shouldBe` True
        curious 3 `shouldBe` False


