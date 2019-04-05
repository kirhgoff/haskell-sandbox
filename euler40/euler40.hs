-- https://projecteuler.net/problem=40
-- Champernowne's constant
-- An irrational decimal fraction is created by concatenating the positive integers:
-- 0.123456789101112131415161718192021...
-- It can be seen that the 12th digit of the fractional part is 1.
-- If dn represents the nth digit of the fractional part, find the value of the following expression.
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

module Euler40 where

import Data.Numbers.Primes
import Data.List

digit :: Integer -> Integer
digit a 
  | a < 10 = a
  | a >= 10 && a < 20 && (a `mod` 2 == 0) = 1
  | a >= 10 && a < 20 && (a `mod` 2 == 1) = a `mod` 10 `quot` 2 
