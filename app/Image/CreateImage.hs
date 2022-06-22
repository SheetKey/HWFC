module Image.CreateImage
  ( gridToImage
  ) where

import WF.Wave

import qualified Graphics.Image as I
import qualified Graphics.Image.IO as I
import qualified Graphics.Image.Interface as I
import Data.Text (Text, unpack)
import Database.Tiles (Tile(tileFilePath))
import Control.Exception (AssertionFailed (AssertionFailed), throw)


gridToPaths :: Grid -> Maybe [[Text]]
gridToPaths grid = do
  tiless <- gridToOrderedFG grid
  return $ (map . map) tileFilePath tiless

rowImage :: [Text] -> IO (I.Image I.VU I.RGBA Double)
rowImage [p] = I.readImageRGBA I.VU $ unpack p
rowImage (p:ps) = do
  image1 <- I.readImageRGBA I.VU $ unpack p
  image2 <- rowImage ps
  return $ I.leftToRight image1 image2

gridImage :: [[Text]] -> IO (I.Image I.VU I.RGBA Double)
gridImage [ps] = rowImage ps
gridImage (ps:pss) = do
  image1 <- rowImage ps
  image2 <- gridImage pss
  return $ I.topToBottom image2 image1

gridToImage :: Grid -> IO (I.Image I.VU I.RGBA Double)
gridToImage grid = do
  case gridToPaths grid of
    Nothing -> throw $ AssertionFailed "Grid wrong size."
    Just pss -> gridImage pss
    
