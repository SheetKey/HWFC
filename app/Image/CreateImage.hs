module Image.CreateImage
  ( gridToImage
  ) where

import WF.Wave ( gridToOrderedFG, Grid )

import qualified Graphics.Image as I
import qualified Graphics.Image.IO as I
import qualified Graphics.Image.Interface as I
import Data.Text (Text, unpack)
import Database.Tiles (Tile(tileFilePath, tileRotation))
import Control.Exception (AssertionFailed (AssertionFailed), throw)

tileImage :: Tile -> IO (I.Image I.VU I.RGBA Double)
tileImage t = case tileRotation t of
                0 -> I.readImageRGBA I.VU $ unpack $ tileFilePath t
                90 -> I.rotate270 <$> I.readImageRGBA I.VU (unpack $ tileFilePath t)
                180 -> I.rotate180 <$> I.readImageRGBA I.VU (unpack $ tileFilePath t)
                270 -> I.rotate90 <$> I.readImageRGBA I.VU (unpack $ tileFilePath t)
                _ -> throw $ AssertionFailed "Bad angle found."

rowImage :: [Tile] -> IO (I.Image I.VU I.RGBA Double)
rowImage [t] = tileImage t
rowImage (t:ts) = do
  image1 <- tileImage t
  image2 <- rowImage ts
  return $ I.topToBottom image2 image1

gridImage :: [[Tile]] -> IO (I.Image I.VU I.RGBA Double)
gridImage [ps] = rowImage ps
gridImage (ps:pss) = do
  image1 <- rowImage ps
  image2 <- gridImage pss
  return $ I.leftToRight image1 image2


gridToImage :: Grid -> IO (I.Image I.VU I.RGBA Double)
gridToImage grid = do
  case gridToOrderedFG grid of
    Nothing -> throw $ AssertionFailed "Grid wrong size."
    Just pss -> gridImage pss

