module Limit where

import Data.List
import Complex

data Limit = Infinity Integer | Stable

projection :: (Complex -> Complex) -> Complex -> [Complex]
projection func z = z : projection func (func z)

limit_checked :: (z -> Bool) -> [z] -> [Bool]
limit_checked pred proj = map pred proj 

indexed :: [z] -> [(Integer, z)]
indexed proj = zip [0..] proj

first_acceptable :: (a -> Bool) -> [a] -> a
first_acceptable pred ips = head (dropWhile (\x -> not (pred x)) ips)

-- is_infitine :: Int -> (a -> Bool) -> [a] -> Bool 
-- is_infitine max_index pred ips =  not (pred (head (drop max_index ips)))

-- head (dropWhile (\(i,z) -> modulo z < 26) (zip [0..] (projection (add (Complex 1 1)) (Complex 0 0))))
-- limit :: Integer -> (Complex -> Bool) -> Projection -> Limit
-- limit cutoff project = takeWhile [(i, x) | (i, x) <- zip [0..] project]
