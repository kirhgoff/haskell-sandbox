module AsciiRenderer where

import Data.Array

prod_ascii_cells = listArray (0, 4) [' ', '.', '_', '=', 'W']

limit_to_color :: Maybe Integer -> Integer -> Char
limit_to_color limit max_index = case limit of
  Nothing -> ascii_cells!0
  Just x -> ascii_cells!index where
    len = fromIntegral $ length ascii_cells
    index = round $ len * (fromIntegral x / fromIntegral max_index)

prod_conversion = Conversion {
  screen_width=40,
  screen_height = 20,
  origin_x = -2.0,
  origin_y = -2.0,
  real_width = 4.0,
  real_height = 4.0
}

render :: (Complex -> Complex) -> Conversion -> Integer -> String
render func conversion max_index = map limit_to_color all_cells where
  dots = [(x, y) | x <- [0..(screen_width prod_conversion)-1], y <- [0..(screen_height prod_conversion)-1]
  real_dots = map screen_to_real dots
  limits =
