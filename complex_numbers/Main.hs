module Main where

import AsciiRenderer

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
  ascii_cells = listArray (0, 4) [' ', '.', '_', '=', 'W']
  fractal = render (\z -> z^2 + (Complex 0.2 0.4)) conversion 200
