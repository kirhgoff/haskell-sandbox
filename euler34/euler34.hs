-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.
module Euler34 where

factorial :: Integer -> Integer
factorial a 
  | a == 0 = 1
  | a == 1 = 1
  | otherwise = a * factorial (a -1)

digits :: Integer -> [Integer]
digits a = reverse (digitsInternal a)

digitsInternal a
  | a < 10 = [a]
  | otherwise = q : digitsInternal r 
    where (r, q) = quotRem a 10

digiFactoSum a = foldl (+) 0 (map factorial (digits a))

curious a = a == digiFactoSum a

filterCurious n = filter curious (take n $ iterate succ 3)

result = sum (filterCurious 100000)