-- The number, 197, is called a circular prime because 
-- all rotations of the digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?
module Euler35 where

import Data.Numbers.Primes
import Data.List

-- TODO copy paste from Euler 34 - refactor
digits :: Integer -> [Integer]
digits a = reverse (digitsInternal a)
digitsInternal a
  | a < 10 = [a]
  | otherwise = q : digitsInternal r 
    where (r, q) = quotRem a 10

combine :: [Integer] -> Integer
combine list = foldl (\a b -> a * 10 + b) 0 list

-- https://stackoverflow.com/questions/7631664/how-to-define-a-rotates-function
listRotations :: [a] -> [[a]]
listRotations l = init (zipWith (++) (tails l) (inits l))

-- TODO read about ad hoc polimorphism
rotations :: Integer -> [Integer]
rotations a = nub (map combine (listRotations (digits a)))

allAre :: (Integer -> Bool) -> [Integer] -> Bool
allAre func [] = True -- not logical?
allAre func (x:xs)
  | func(x) == False = False
  | otherwise = allAre func xs

circularPrime :: Integer -> Bool
circularPrime 1 = False
circularPrime a 
  | not (isPrime a) = False
  | otherwise = allAre isPrime (rotations a)

result = length(filter circularPrime (takeWhile (<1000000) [1..]))