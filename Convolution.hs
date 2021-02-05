module Convolution (convolve2d, convolveDebug) where

import Data.List.Split
import Debug

type Rect = [[Int]]

-- find all sub n square in a big rect
subMatrix :: Rect -> Int -> [[Rect]]
subMatrix r size = chunksOf ((length r ) - size + 1) [row x y | x<-range, y<-range]
        where
           range = [0..(length r - size)]
           row x y = take size $ drop y $ col x y
           col x y = [take size (drop x v) | v <- r]

paddingMatrix :: Rect -> Int -> Rect
paddingMatrix r psize = ptop ++ pside ++ ptop
         where
            rsize = length (r !! 0) + psize + psize
            ptop = take psize $ repeat $ take rsize $ repeat 0
            pside1 = take psize $ repeat 0
            pside2 x = pside1 ++ x ++ pside1
            pside = [pside2 v | v <- r]

           
-- zip all sub square with the kernel square
-- sum each new square

-- convolve input -> kernel -> output
convolve2d :: Rect -> Rect -> Rect
convolve2d m k = [[ s (zip' mtx k)  | mtx <- row] | row  <- subm ]
        where
            s mtx = sum $ [ sum ([a*b | (a,b) <- row ] ) | row <- mtx ]
            zip' mtx ker = zipWith zip mtx ker
            padm = paddingMatrix m $ length k `div` 2
            subm = subMatrix padm $ length k


convolveDebug = do
  let sq = chunksOf 4 [1..4*4]

  -- print res

  -- print size


  let pad = paddingMatrix sq 1

  print pad
  table pad

  let res2 = subMatrix pad 3

  print res2
  table res2

  let size = (length res2, length (res2 !! 0))
  print size

  let conv = convolve2d sq [[1,1,1],[1,0,1],[1,1,1]]
  
  print conv
