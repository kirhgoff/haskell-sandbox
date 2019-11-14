import Test.Hspec
import Complex
import Limit

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

    describe "indexed_projection" $ do
      it "builds the sequence of indexed numbers" $ do
        take 3 (projection (add (Complex 1 2)) (Complex 0 0))  `shouldBe` [(Complex 0 0), (Complex 1 2), (Complex 2 4)]

    describe "first_acceptable" $ do
      it "returns index of first acceptable element" $ do
        first_acceptable (\z -> (modulo z) > 4) (projection (add (Complex 1 1)) (Complex 0 0))  `shouldBe` 2
