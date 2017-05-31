-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Coins

main :: IO ()
main = hspec $ do
  describe "Coins" $ do
    describe "CoinSet" $ do
      it "should implement Show" $ do
        show (CoinSet 5 1) `shouldBe` "5x1"        
      it "should implement Eq" $ do
        (CoinSet 5 10) == (CoinSet 5 10) `shouldBe` True
        (CoinSet 3 10) == (CoinSet 5 10) `shouldBe` False
        (CoinSet 5 8) == (CoinSet 5 10) `shouldBe` False
      it "should implement Ord" $ do
        (CoinSet 5 10) > (CoinSet 3 10) `shouldBe` True
        (CoinSet 5 10) > (CoinSet 5 9) `shouldBe` True
        (CoinSet 3 10) < (CoinSet 5 9) `shouldBe` True

