-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example the 5-digit number 15234 is 1 through 5 pandigital.
-- The product 7254 is unusual as the identity 39 × 186 = 7254 containing multiplicand multiplier and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
module Euler32 where

import Data.List
import Data.Maybe
import Debug.Trace

debug = flip trace

variants = permutations ([1..9] ++ [666, 777])

fromDigits xs = foldl addDigit 0 xs
   where addDigit num d = 10*num + d

-- Final check
isValidFinal [] _ _ = Nothing
isValidFinal _ [] _ = Nothing
isValidFinal _ _ [] = Nothing
isValidFinal ns ms ps = 
  let 
    n = fromDigits ns
    m = fromDigits ms
    p = fromDigits ps
  in 
    if n * m == p 
    then Just p 
    else Nothing

-- Parsing
isValidParsing :: [Integer] -> [Integer] -> Bool -> [Integer] -> Bool -> [Integer] -> Maybe Integer
isValidParsing (666:xs) [] _ _ _ _ = Nothing
isValidParsing (777:xs) _ _ [] _ _ = Nothing
isValidParsing (777:[]) _ _ _ _ _ = Nothing

isValidParsing (x:xs) ns multiply ms equal ps 
  | x == 666 = isValidParsing xs ns True ms equal ps
  | x == 777 = isValidParsing xs ns multiply ms True ps
  | not multiply = isValidParsing xs (ns ++ [x]) multiply ms equal ps
  | multiply && not equal = isValidParsing xs ns multiply (ms ++ [x]) equal ps
  | equal = isValidParsing xs ns multiply ms equal (ps ++ [x])

isValidParsing [] ns plus ms equal ps 
  | plus && equal = isValidFinal ns ms ps
  | otherwise = Nothing

isValid xs = isValidParsing xs [] False [] False []

validsMaybe xs = map isValid (permutations xs) 
valids xs = nub(mapMaybe id (validsMaybe xs))
resultSum xs = sum (valids xs)

-- *Euler32 Data.Monoid Data.Maybe> sum([7632,6952,7852,7254,5346,5796,4396])
-- 45228
