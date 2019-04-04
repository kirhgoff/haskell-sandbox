module CoinSums where

coins = [0, 1, 2, 5, 10, 20, 50, 100, 200]

firstDenomination x = coins !! x

countChange amount = go amount (length coins - 1)
  where go amt kindsOfCoins
          | amt == 0 = 1
          | amt < 0 || kindsOfCoins == 0 = 0
          | otherwise = (go amt (kindsOfCoins - 1)) + (go (amt - (firstDenomination kindsOfCoins)) kindsOfCoins)

main = putStrLn $ "" ++ show (countChange 200)