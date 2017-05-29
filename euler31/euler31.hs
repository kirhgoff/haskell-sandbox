-- https://projecteuler.net/problem=31
-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

removeItem _ [] = []
removeItem x (y:ys) | x == y  = removeItem x ys
                    | otherwise = y : removeItem x ys

maximumCount amount coin = div amount coin

variations :: Integer -> [Integer] -> [[(Integer, Integer)]]
variations amount _ | amount <= 0 = [[]]
variations amount coins = 
  let permutations = [(coin, count) | coin <- coins, count <- [0..(maximumCount amount coin)]]
  in map (\(x, y) -> (x, y) : concat (variations (amount - x * y) (removeItem x coins))) permutations


  