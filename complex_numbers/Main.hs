module Main where

import AsciiRenderer
import Conversion
import Data.Array
import Data.List
import Data.List.Split
import Complex

main :: IO ()
main = putStrLn fractal where
  conversion = Conversion {
    screen_width=40,
    screen_height = 20,
    origin_x = -2.0,
    origin_y = -2.0,
    real_width = 4.0,
    real_height = 4.0
  }
  ascii_cells = listArray (0, 4) ['W', '=', '_', '.', ' ']
  symbols = render (\z -> z^2 + (Complex 0.274 (-0.008))) conversion 200 ascii_cells
  fractal = intercalate "\n" $ chunksOf 40 symbols
