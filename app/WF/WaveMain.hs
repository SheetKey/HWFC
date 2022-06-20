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
      printGrid, Length, Width, CellInfo (cellEntropy) )
import WF.Propagate ( propagate, debugPropagate )
import Utils
import Database.Tiles

import qualified Data.Vector as V
import System.Random (randomRIO)
import GHC.IO.Encoding.UTF16 (utf16_encode)

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
  let newGrid = propagate [cell] w l $ updateGrid n cell grid
  printGrid newGrid
  let minEnt = minEntropy newGrid
  myPutStr "Min entropy: "
  print minEnt 
  let indices = entropyIndices minEnt newGrid
      indicesNum = V.length indices - 1
  print indicesNum
  index <- randomRIO (0, indicesNum)
  let newCell = indices V.! index
  print newCell
  newerGrid <- updateTile newCell newGrid
  printGrid newerGrid
  putStrLn "newestGrid"
  let newestGrid = propagate [newCell] w l newerGrid
  printGrid newestGrid

testWave = do
  myPutStr "Horizontal number of tiles (width): "
  w <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  l <- intGetLine
  tiles <- getAllTiles
  let grid = initialGrid w l tiles
  newGrid <- debugWave w l grid
  printGrid newGrid

testWave2 = do
  tiles <- getAllTiles
  let grid = initialGrid 4 5 tiles
  newGrid <- debugWave 4 5 grid
  printGrid newGrid


debugWave :: Width -> Length -> Grid -> IO Grid
debugWave w l grid = do
  -- Determine the minimun entropy in the grid
  myPutStr "minEntropy grid: "
  print $ minEntropy grid
  case minEntropy grid of
    -- When min entropy is 0 then its done
    0 -> do
      putStrLn "Finished!"
      return grid
    x -> do
      myPutStr "entropyIndices x grid: "
      print $ entropyIndices x grid
      myPutStr "V.length indices - 1: "
      print $ V.length (entropyIndices x grid) - 1
      let indices = entropyIndices x grid
          indicesNum = V.length indices - 1
      index <- randomRIO (0, indicesNum)
      myPutStr "index: "
      print index
      let cell = indices V.! index
      -- Collapse the cell and create a new grid
      newGrid <- updateTile cell grid
      myPutStr "newGrid: "
      printGrid newGrid
      putStrLn "debugPropagate [cell] w l newGrid: "
      newerGrid <- debugPropagate [cell] w l newGrid
      myPutStr "newerGrid: "
      printGrid newerGrid
      -- Propagate the wave through the new grid, updating all cells
      debugWave w l newerGrid

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
