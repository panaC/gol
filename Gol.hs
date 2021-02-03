module Gol (run) where

import Convolution (convolve2d)

run :: [[Int]] -> [[Int]] -> [[Int]]
run i k = [[ deadOrAlive neigh value | (neigh, value) <- row ] | row <- zip']
    where
      conv = convolve2d i k
      zip' = zipWith zip conv i
      deadOrAlive :: Int -> Int -> Int
      deadOrAlive neigh value
          | neigh == 3 = 1
          | neigh == 2 = value
          | otherwise = 0

