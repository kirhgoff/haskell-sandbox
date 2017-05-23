-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13

-- It can be verified that the sum of the numbers on the diagonals is 101.
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
-- [1]
-- [2, *3, 4, *5, 6, *7, 8, *9]
-- [10, 11, 12, *13, 14, 15, 16, *17, 18, 19, 20, *21, 22, 23, 24, *25]

-- [1] 
-- [], [1], []
-- [], [1, 2], [] 
-- [], [1, 2], [5, 4, 3]
-- [], [6, 1, 2], [5, 4, 3]
-- [7, 8, 9], [6, 1, 2], [5, 4, 3]

-- [7, 8, 9], [6, 1, 2], [5, 4, 3]
-- [], [7, 8, 9, 10], [6, 1, 2], [5, 4, 3], [] 
-- [], [7, 8, 9, 10], [6, 1, 2, 11], [5, 4, 3], []
-- [], [7, 8, 9, 10], [6, 1, 2, 11], [5, 4, 3, 12], [17, 16, 15, 14, 13]
-- [], [7, 8, 9, 10], [6, 1, 2, 11], [18, 5, 4, 3, 12], [17, 16, 15, 14, 13]
-- [], [7, 8, 9, 10], [19, 6, 1, 2, 11], [18, 5, 4, 3, 12], [17, 16, 15, 14, 13]
-- [], [20, 7, 8, 9, 10], [19, 6, 1, 2, 11], [18, 5, 4, 3, 12], [17, 16, 15, 14, 13]
-- [21, 22, 23, 24, 25], [20, 7, 8, 9, 10], [19, 6, 1, 2, 11], [18, 5, 4, 3, 12], [17, 16, 15, 14, 13]

-- 1. append last element
-- 2. add last row - reverse
-- 3. prepend first element
-- 4. add first row

spiralAppend :: Integer -> [[Integer]] -> [[Integer]]
spiralAppend n [[]] = [[]]
spiralAppend n [r] = [r ++ [n]]
spiralAppend n (r : rs) = (r ++ [n]) : spiralAppend (n + 1) rs

-- spiral :: Int -> [[Int]]
-- spiral 0 = [[1]] 
-- spiral radius = 
-- 	let (r : rs) = spiral (radius - 1)    
-- 	in spiralAppend (r rs)



