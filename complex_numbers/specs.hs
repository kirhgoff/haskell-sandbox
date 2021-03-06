import Test.Hspec
import Complex
import Limit
import AsciiRenderer
import Data.Array

main :: IO ()
main = hspec $ do
  describe "Complex" $ do
    describe "real" $ do
      it "return real part of a complex number" $ do
        real (Complex 12 23)  `shouldBe` 12

    describe "imag" $ do
      it "return image part of a complex number" $ do
        imag (Complex 12 23)  `shouldBe` 23

    describe "add" $ do
      it "adds two complex numbers" $ do
        add (Complex 12 23) (Complex (-12) 43)  `shouldBe` (Complex 0 66)
        (Complex 12 23) + (Complex (-12) 43)  `shouldBe` (Complex 0 66)

    describe "mul" $ do
      it "multiplies two complex numbers" $ do
        mul (Complex 3 2) (Complex 1 7)  `shouldBe` (Complex (-11) 23)
        (Complex 3 2) * (Complex 1 7)  `shouldBe` (Complex (-11) 23)

    describe "square" $ do
      it "squares the complex number" $ do
        square (Complex 1 1)  `shouldBe` Complex 0 2
        (Complex 1 1)^2  `shouldBe` Complex 0 2

    describe "modulo" $ do
      it "gives the squared magnitude of a complex number" $ do
        modulo (Complex 3 4)  `shouldBe` 25

  describe "Limit" $ do
    describe "projection" $ do
      it "builds the sequence of numbers" $ do
        take 3 (projection (add (Complex 1 2)) (Complex 0 0))  `shouldBe` [(Complex 0 0), (Complex 1 2), (Complex 2 4)]

    describe "limit_checked" $ do
      it "convert sequence of elements to boolean" $ do -- TODO try to redo it with where
        take 3 (limit_checked (\z -> (modulo z) > 4) (projection (add (Complex 1 1)) (Complex 0 0)))  `shouldBe` [False, False, True]

-- TODO ask David Overtone about code style
    describe "indexed" $ do
      it "builds the sequence of indexed numbers" $ do
        take 3 (indexed (limit_checked ((>10) . modulo) (projection (add (Complex 1 2)) (Complex 0 0))))  `shouldBe` [(0, False), (1, False), (2, True)]

    describe "only_for" $ do
      it "returns a part of projection" $ do
        only_for 3 (projection (+1) 0)  `shouldBe` [0, 1, 2]

    describe "limit" $ do
      it "returns a limit" $ do
        limit 0 (+1) 10 (>5)  `shouldBe` Just 6
        limit 0 (+1) 10 (>20)  `shouldBe` Nothing

  describe "AsciiRenderer" $ do
    describe "limit_to_color" $ do
      it "return presentation of limit" $ do
        let chars = listArray (0, 4) ['W', '=', '_', '.', ' '] 
        limit_to_color Nothing chars 10  `shouldBe` 'W'
        limit_to_color (Just 0) chars 10  `shouldBe` 'W'
        limit_to_color (Just 1) chars 10  `shouldBe` 'W'
        limit_to_color (Just 2) chars 10  `shouldBe` '='
        limit_to_color (Just 3) chars 10  `shouldBe` '='
