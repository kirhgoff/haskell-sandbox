module AsciiRenderer where

import Complex
import Limit
import Conversion
import Data.Array

limit_to_color :: Maybe Int -> Array -> Char
limit_to_color limit ascii_cells = case limit of
  Nothing -> ascii_cells!0
  Just x -> ascii_cells!index where
    len = fromIntegral $ length ascii_cells
    index = round $ len * (fromIntegral x / len)

render :: (Complex -> Complex) -> Conversion -> Int -> [Char]
render func conversion max_index = map limit_to_color limits  where
  dots = [(fromIntegral x, fromIntegral y) | x <- [0..(screen_width conversion)-1], y <- [0..(screen_height conversion)-1]]
  real_dots = map (\d -> screen_to_real d conversion) dots
  limits = map (\z -> limit z func max_index (\z -> (modulo z) > 2.0)) real_dots
