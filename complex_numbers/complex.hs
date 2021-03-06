module Complex where

data Complex = Complex Double Double deriving (Show, Eq)

real :: Complex -> Double
real (Complex re _) = re

imag :: Complex -> Double
imag (Complex _ im) = im

add :: Complex -> Complex -> Complex
add (Complex re1 im1) (Complex re2 im2) = Complex (re1 + re2) (im1 + im2)

mul :: Complex -> Complex -> Complex
mul (Complex re1 im1) (Complex re2 im2) = Complex (re1 * re2  - im1 * im2) (re2 * im1 + re1 * im2)

square :: Complex -> Complex
square a = mul a a

modulo :: Complex -> Double
modulo (Complex re im) = re^2 + im^2 

instance Num Complex where
   a + b = add a b 
   a * b = mul a b 
   (Complex a b) - (Complex c d) = Complex (a-c) (b-d)
   abs    (Complex a b) = Complex (abs a) (abs b) 
   signum (Complex a b) = Complex (signum a) (signum b)
   fromInteger i = Complex (fromInteger i) (fromInteger i)
