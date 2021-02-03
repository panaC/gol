module Main where

import Convolution (convolveDebug) 

main :: IO ()
main = do
--  let convolve = convolve2d [[1,2,3], [4,5,6], [7,8,9]] [[-1,-2,-1], [0,0,0], [1,2,1]]


  putStrLn "DEBUG"

  convolveDebug

  --print convolve

