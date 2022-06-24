{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module MainOptions
  ( myMain
  ) where 

import WF.WaveMain 
import WF.Wave
import Database.Tiles

import Control.Monad (when)
import Options (Options, defineOptions, simpleOption, runCommand)
import qualified Data.Map as M
import qualified Graphics.Image.IO as I
import Image.CreateImage (gridToImage)


data MainOptions = MainOptions
  { optCreate :: Bool
  , optAdd :: Bool
  , optPrint :: Bool
  , optWeight :: Bool
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "create" False
        "Create the database."
    <*> simpleOption "add" False
        "Add a tile to the database."
    <*> simpleOption "print" False
        "Print the database."
    <*> simpleOption "weight" False
        "Change the weight of a given tile."

myMain :: IO ()
myMain = runCommand $ \opts args -> do 
  when (null args) generateWave
  when (optCreate opts) createDatabase
  when (optAdd opts) insertNewTile
  when (optPrint opts) printDatabase
  when (optWeight opts) changeWeight
  

generateWave :: IO ()
generateWave = do
  grid <- waveMain
  putStrLn "Grid: "
  print $ M.map tileId <$> finalGrid grid
  putStrLn "gridToOrderedFG: "
  print $ (map . map) tileId <$> gridToOrderedFG grid
  image <- gridToImage grid
  I.writeImage "testImage.png" image
