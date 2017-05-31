-- https://projecteuler.net/problem=31
-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

removeItem _ [] = []
removeItem x (y:ys) | x == y  = removeItem x ys
                    | otherwise = y : removeItem x ys

maximumCount amount coin = div amount coin

variations :: Integer -> [Integer] -> [Set(Integer, Integer)]
variations amount coins = permutate amount coins Set.empty

permutate :: Integer -> [Integer] -> Set (Integer,Integer) -> [Set(Integer, Integer)]
permutate amount coins results 
  | amount <= 0 && not (null (coins)) = [Set.empty]
  | amount > 0 && null coins = [Set.empty]
  | amount == 0 && null coins = [results]

permutate amount coins results =
  let 
    permutations = [(coin, count) | coin <- coins, count <- [0..(maximumCount amount coin)]]
    eachOne (x,y) = permutate (amount - x * y) (removeItem x coins) (Set.insert (x,y) results)
    solutions = concat(map eachOne permutations)
  in 
    filter (not . Set.null) solutions

prettyPrint m = intercalate "\n" (map (intercalate "\t") mstr)
  where mstr = map (map show) m

-- Another approach with data types 
-- Need to remove duplication branches

type Coin = Integer 
data CoinSet = CoinSet {
  face :: Coin,
  count :: Integer
}
data Change = Change (Set CoinSet)

instance Show CoinSet where
  show (CoinSet face count) = show face ++ "x" ++ show count

instance Ord CoinSet where
  compare = (comparing face) `mappend` (comparing count)