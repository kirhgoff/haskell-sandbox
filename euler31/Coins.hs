module Coins where

import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------
type Coin = Integer 
data CoinSet = CoinSet {
  face :: Coin,
  count :: Integer
}

instance Show CoinSet where
  show (CoinSet face count) = show face ++ "x" ++ show count

instance Eq CoinSet where
  x == y = (face x == face y && count x == count y)

instance Ord CoinSet where
  compare = (comparing face) `mappend` (comparing count)

----------------------------------------
type Change = Set CoinSet


-- instance Ord CoinSet where
--   compare = (comparing face) `mappend` (comparing count)