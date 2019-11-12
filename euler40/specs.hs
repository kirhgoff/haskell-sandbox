-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler40

-- 0.123456789101112131415161718192021...

main :: IO ()
main = hspec $ do
  describe "Euler40" $ do
    describe "digit" $ do
      it "return correct digit in simple case" $ do
        digit 1 `shouldBe` 1
        digit 2 `shouldBe` 2
        digit 3 `shouldBe` 3
        digit 9 `shouldBe` 9

      it "return correct digit in more complex case" $ do
        digit 10 `shouldBe` 1
        digit 11 `shouldBe` 0
        digit 12 `shouldBe` 1
        digit 13 `shouldBe` 1
        digit 14 `shouldBe` 1
        digit 15 `shouldBe` 2
        digit 16 `shouldBe` 1
        digit 17 `shouldBe` 3
        digit 18 `shouldBe` 1
        digit 19 `shouldBe` 4

      it "return correct digit in more more complex case" $ do
        digit 20 `shouldBe` 1
        digit 21 `shouldBe` 5
        digit 22 `shouldBe` 1
        digit 23 `shouldBe` 6
        digit 24 `shouldBe` 1
        digit 25 `shouldBe` 7
        digit 26 `shouldBe` 1
        digit 27 `shouldBe` 8
        digit 28 `shouldBe` 1
        digit 29 `shouldBe` 9