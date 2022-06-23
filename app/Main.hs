module Main where

import WF.WaveMain (waveMain)
import qualified Graphics.Image.IO as I
import qualified Data.Map as M
import Image.CreateImage (gridToImage)
import WF.Wave (gridToOrderedFG, finalGrid)
import Database.Tiles (Tile(tileId))
import Data.Maybe (isJust)

main :: IO ()
main = do
  grid <- waveMain
  putStrLn "Grid: "
  print $ M.map tileId <$> finalGrid grid
  putStrLn "gridToOrderedFG: "
  print $ (map . map) tileId <$> gridToOrderedFG grid
  image <- gridToImage grid
  I.writeImage "testImage.png" image
