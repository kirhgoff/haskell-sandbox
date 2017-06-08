-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Euler32

main :: IO ()
main = hspec $ do
  describe "Euler32" $ do
    describe "isValidFinal" $ do
      it "should work" $ do
        isValidFinal [2] [3] [6] `shouldBe` Just 6        
        isValidFinal [2, 1] [3] [6, 3] `shouldBe` Just 63
        isValidFinal [2] [3, 1] [6, 2] `shouldBe` Just 62
        isValidFinal [2] [3, 1] [6, 3] `shouldBe` Nothing

    describe "isValidFinal" $ do
      it "should work" $ do
        isValidFinal [2] [3, 1] [6, 3] `shouldBe` Nothing
