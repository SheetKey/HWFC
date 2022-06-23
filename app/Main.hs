module Main where

import WF.WaveMain (waveMain)
import qualified Graphics.Image.IO as I
import Image.CreateImage (gridToImage)

main :: IO ()
main = do
  grid <- waveMain
  print grid
  image <- gridToImage grid
  I.writeImage "testImage.png" image
