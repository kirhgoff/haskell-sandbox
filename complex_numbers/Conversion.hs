module Conversion where

import Limit
import Complex

data Conversion = Conversion { 
  screen_width :: Int, 
  screen_height :: Int, 
  origin_x :: Double, 
  origin_y :: Double, 
  real_width :: Double, 
  real_height :: Double 
}

cell_width :: Conversion -> Double
cell_width conversion = (real_width conversion) / fromIntegral (screen_width conversion)

cell_height :: Conversion -> Double
cell_height conversion = (real_height conversion) / fromIntegral (screen_height conversion)

real_x :: Integer -> Conversion -> Double
real_x x conversion = (origin_x conversion) +  (fromIntegral x * (cell_width conversion))  

real_y :: Integer -> Conversion -> Double
real_y y conversion = (origin_y conversion) + (fromIntegral y * (cell_height conversion))  

screen_to_real :: (Integer, Integer) -> Conversion -> Complex 
screen_to_real (i, j) conversion = Complex (real_x i conversion) (real_y j conversion)
