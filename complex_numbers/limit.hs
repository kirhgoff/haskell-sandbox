module Limit where

import Data.List
import Complex

data Limit = Infinity Integer | Stable

projection :: (Complex -> Complex) -> Complex -> [Complex]
projection func z = z : projection func (func z)

--- First approach
indexed_projection :: (Complex -> Complex) -> Complex -> [(Integer, Complex)]
indexed_projection func z = zip [0..] (projection (add (Complex 1 1)) (Complex 0 0))

first_acceptable :: ((Integer, Complex) -> Bool) -> [(Integer, Complex)] -> (Integer, Complex)
first_acceptable pred ips = head (dropWhile (\x -> not (pred x)) ips)

is_infitine :: Int -> ((Integer, Complex) -> Bool) -> [(Integer, Complex)] -> Bool 
is_infitine max_index pred ips =  not (pred (head (drop max_index ips)))

-- head (dropWhile (\(i,z) -> modulo z < 26) (zip [0..] (projection (add (Complex 1 1)) (Complex 0 0))))
-- limit :: Integer -> (Complex -> Bool) -> Projection -> Limit
-- limit cutoff project = takeWhile [(i, x) | (i, x) <- zip [0..] project]
