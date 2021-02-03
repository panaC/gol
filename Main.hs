module Main where

import Convolution (convolveDebug) 
import Gol (run)
import Debug


main :: IO ()
main = do
--  let convolve = convolve2d [[1,2,3], [4,5,6], [7,8,9]] [[-1,-2,-1], [0,0,0], [1,2,1]]


  putStrLn "DEBUG"

  -- convolveDebug

  putStrLn "RUN"

  let kernel = [[1,1,1],[1,0,1],[1,1,1]]
  let input = [[0,1,0,1,0,0,1,1,0,1,0,0,0,1] | _ <- [0..13]]

  table input

  putStrLn "ROUND"

  let r1 = run input kernel

  table r1


  putStrLn "ROUND"

  let r2 = run r1 kernel

  table r2
  putStrLn "ROUND"

  let r3 = run r2 kernel

  table r3
  putStrLn "ROUND"

  let r4 = run r3 kernel

  table r4
  putStrLn "ROUND"

  let r5 = run r4 kernel

  table r5
  putStrLn "ROUND"

  let r6 = run r5 kernel

  table r6
  putStrLn "ROUND"

  let r7 = run r6 kernel

  table r7
  putStrLn "ROUND"

  let r8 = run r7 kernel

  table r8
