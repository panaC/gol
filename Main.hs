{-# LANGUAGE OverloadedStrings #-}
module Main where

import Convolution (convolveDebug) 
import Gol (run)
import Debug

import Codec.BMP
import qualified Data.ByteString 

--import System.Exit (exitWith, ExitCode(..))
--import Control.Concurrent (threadDelay)

import Graphics.UI.GLUT

import Foreign.Marshal.Array
import Foreign.Storable
import Data.Int

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  -- renderPrimitive Points $
  --  mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  -- flush

  let arr = [255 | x <- [0..(256*256-1)]] :: [Int8]

  Right bmp  <- readBMP "frog.bmp"
  let rgba   =  unpackBMPToRGBA32 bmp
  let (width, height) = bmpDimensions bmp

  print (width, height)
  let dat = Data.ByteString.unpack rgba

  print $ length dat
  print $ (*) width height 
  print $ (*) 4 $ (*) width height 
  print dat

  --ptr <- newArray arr
  ptr <- newArray dat

  print ptr

  --drawPixels (Size 256 256) (PixelData Luminance UnsignedByte ptr)
  let width32 = fromIntegral width :: Int32
  let height32 = fromIntegral height :: Int32
  let imgSize =  (Size (width32) (height32))
  drawPixels imgSize (PixelData RGBA UnsignedByte ptr)

  windowSize $= imgSize

  flush
 
{-
run :: IO ()
run = go
  where
    go = do
      
      flush
      addTimerCallback 10 $ go

-}

{-
import SDL
--import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
      qPressed = any eventIsQPress events
  --rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)

-}

--  threadDelay (10 * 1000000)
  --  exitWith ExitSuccess

{-

  main :: IO ()
    main = do
    --  let convolve = convolve2d [[1,2,3], [4,5,6], [7,8,9]] [[-1,-2,-1], [0,0,0], [1,2,1]]


    Right bmp  <- readBMP "frog.bmp"
    let rgba   =  unpackBMPToRGBA32 bmp
    let (width, height) = bmpDimensions bmp

    print (width, height)
    let dat = Data.ByteString.unpack rgba

    print $ length dat
    print $ (*) width height 
    print $ (*) 4 $ (*) width height 


    putStrLn "DEBUG"
    -}

    -- convolveDebug
{-

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
    -}

