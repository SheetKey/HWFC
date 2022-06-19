module WF.WaveMain where

import WF.Wave
    ( minEntropy,
      updateGrid,
      chooseTile,
      chooseCell,
      initialGrid,
      Grid,
      entropyIndices,
      updateTile,
      printGrid, Length, Width )
import WF.Propagate ( propagate )
import Utils
import Database.Tiles

import qualified Data.Vector as V
import System.Random (randomRIO)

-- A method to choose the first tile.
-- Eventually add alternative methods that allow user
-- to choose the first tile.
firstCollapse :: IO ((Width, Length), Grid)
firstCollapse = do
  myPutStr "Horizontal number of tiles (width): "
  w <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  l <- intGetLine
  tiles <- getAllTiles
  let grid = initialGrid w l tiles
  cell <- chooseCell w l
  n <- chooseTile $ length tiles
  return ((w,l),propagate [cell] w l $ updateGrid n cell grid)

test = do
  myPutStr "Horizontal number of tiles (width): "
  w <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  l <- intGetLine
  tiles <- getAllTiles
  let grid = initialGrid w l tiles
  cell <- chooseCell w l
  n <- chooseTile $ length tiles
  printGrid $ propagate [cell] w l $ updateGrid n cell grid


-- THE WAVE FUNCTION!!!!!
-- Repeat until all entropy is 0 or failure.
wave :: Width -> Length -> Grid -> IO Grid
wave w l grid = do
  -- Determine the minimun entropy in the grid
  case minEntropy grid of
    -- When min entropy is 0 then its done
    0 -> do
      putStrLn "Finished!"
      return grid
    x -> do
      let indices = entropyIndices x grid
          indicesNum = V.length indices - 1
      index <- randomRIO (0, indicesNum)
      let cell = indices V.! index
      -- Collapse the cell and create a new grid
      newGrid <- updateTile cell grid
      -- Propagate the wave through the new grid, updating all cells
      wave w l $ propagate [cell] w l newGrid

-- CALL THIS
waveMain :: IO ()
waveMain = do
  trio <- firstCollapse
  finalGrid <- uncurry (uncurry wave) trio
  printGrid finalGrid
