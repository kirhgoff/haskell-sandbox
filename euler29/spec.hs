-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Spiral

main :: IO ()
main = hspec $ do
  describe "spiral" $ do
  	describe "spiralAppend" $ do
	    it "works for one row" $ do
	      spiralAppend 2 [[1]] `shouldBe` [[1, 2]]
	      spiralAppend 3 [[1, 2]] `shouldBe` [[1, 2, 3]]

	    it "works for two rows" $ do
	      spiralAppend 2 [[1], [2]] `shouldBe` [[1, 2], [2, 3]]
	      spiralAppend 5 [[1, 2], [3, 4]] `shouldBe` [[1, 2, 5], [3, 4, 6]]

  	describe "spiralPrepend" $ do
	    it "works for one row" $ do
	      spiralPrepend 2 [[1]] `shouldBe` [[2, 1]]
	      spiralPrepend 3 [[1, 2]] `shouldBe` [[3, 1, 2]]

	    it "works for two rows" $ do
	      spiralPrepend 4 [[1], [2]] `shouldBe` [[4, 1], [3, 2]]
	      spiralPrepend 6 [[1, 2], [3, 4]] `shouldBe` [[6, 1, 2], [5, 3, 4]]

  	describe "spiralTop" $ do
	    it "works for one row" $ do
	      spiralTop 2 [[1]] `shouldBe` [[2], [1]]
	      spiralTop 3 [[1, 2]] `shouldBe` [[3, 4], [1, 2]]
	      spiralTop 4 [[1, 2, 3]] `shouldBe` [[4, 5, 6], [1, 2, 3]]

	    it "works for two rows" $ do
	      spiralTop 4 [[1], [2]] `shouldBe` [[4], [1], [2]]
	      spiralTop 6 [[1, 2], [3, 4]] `shouldBe` [[6, 7], [1, 2], [3, 4]]

  	describe "spiralBottom" $ do
	    it "works for one row" $ do
	      spiralBottom 2 [[1]] `shouldBe` [[1], [2]]
	      spiralBottom 3 [[1, 2]] `shouldBe` [[1, 2], [4, 3]]
	      spiralBottom 4 [[1, 2, 3]] `shouldBe` [[1, 2, 3], [6, 5, 4]]

	    it "works for two rows" $ do
	      spiralBottom 4 [[1], [2]] `shouldBe` [[1], [2], [4]]
	      spiralBottom 6 [[1, 2], [3, 4]] `shouldBe` [[1, 2], [3, 4], [7, 6]]
