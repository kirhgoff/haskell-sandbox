module Limit where

import Data.List
import Complex

data Limit = Infinity Integer | Stable

projection :: (z -> z) -> z -> [z]
projection func z = z : projection func (func z)

limit_checked :: (z -> Bool) -> [z] -> [Bool]
limit_checked pred proj = map pred proj 

indexed :: [z] -> [(Int, z)]
indexed proj = zip [0..] proj

only_for :: Int -> [z] -> [z]
only_for max_index proj = take max_index proj

limit :: z -> (z -> z) -> Int -> (z -> Bool) -> Maybe Int 
limit z func max_index max_pred = elemIndex True (only_for max_index (limit_checked max_pred (projection func z)))
