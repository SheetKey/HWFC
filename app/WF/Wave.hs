module WF.Wave where

import Database.Tiles ( Tile (downConnector, upConnector, rightConnector, leftConnector, tileId), getAllTiles )
import qualified Data.Vector as V
import Utils (myPutStr, intGetLine)
import System.Random (randomRIO)
import Control.Exception (throw, PatternMatchFail (PatternMatchFail), AssertionFailed (AssertionFailed))
import Data.Text (Text)

data CellInfo = CellInfo {
    cellTiles :: V.Vector Tile
  , cellEntropy :: Int
  }

type Row = V.Vector CellInfo
type Grid = V.Vector Row
type Length = Int
type Width = Int
type Cell = (Int, Int)


gridToRow :: Width -> Grid -> Row
gridToRow w g = g V.! w

gridToCell :: Width -> Length -> Grid -> CellInfo
gridToCell w l g = gridToRow w g V.! l

gridToTiles :: Width -> Length -> Grid -> V.Vector Tile
gridToTiles w l g = cellTiles $ gridToCell w l g

gridToTile :: Width -> Length -> Int -> Grid -> Tile
gridToTile w l n g = gridToTiles w l g V.! n

gridToId :: Width -> Length -> Int -> Grid -> Int
gridToId w l n g = tileId $ gridToTile w l n g

gridToNorth :: Width -> Length -> Grid -> Int
gridToNorth w l g = upConnector $ gridToTile w l 0 g

gridToEast :: Width -> Length -> Grid -> Int
gridToEast w l g = rightConnector $ gridToTile w l 0 g

gridToSouth :: Width -> Length -> Grid -> Int
gridToSouth w l g = downConnector $ gridToTile w l 0 g

gridToWest :: Width -> Length -> Grid -> Int
gridToWest w l g = leftConnector $ gridToTile w l 0 g


printGrid :: Grid -> IO ()
printGrid grid = do
  -- DELETE
  putStrLn "2"
  go (V.length grid) (V.length (grid V.! 0)) grid []
  where
    go :: Width -> Length -> Grid -> [Int] -> IO ()
    go 0 0 grid xs = print $ gridToId 0 0 0 grid:xs
    go w 0 gird xs = do
      print $ gridToId w 0 0 grid:xs
      -- DELETE
      putStrLn "3"
      go (w - 1) (V.length (grid V.! 0)) grid []
    go w l grid xs = go w (l - 1) grid $ gridToId w l 0 grid:xs
      
  
  
intersectTiles :: (Width -> Length -> Grid -> Int)
               -> (Tile -> Int)
               -> Width -> Length -> Cell -> Grid -> Grid
intersectTiles gridTo connector w l (x, y) g =
  if V.length newCell2Tiles > 0
  then newGrid
  else throw $ AssertionFailed "No possible tiles to the west."
  where cell1Conn = gridTo w l g
        cell2Tiles = gridToTiles x y g
        func :: Int -> Tile -> Bool
        func conn t = conn == connector t
        newCell2Tiles = V.filter (func cell1Conn) cell2Tiles
        newCell2 = CellInfo newCell2Tiles (V.length newCell2Tiles - 1)
        newRow = (g V.! x) V.// [(y, newCell2)]
        newGrid = g V.// [(x, newRow)]


intersectTilesWest :: Width -> Length -> Cell -> Grid -> Grid
intersectTilesWest = intersectTiles gridToWest rightConnector

intersectTilesEast :: Width -> Length -> Cell -> Grid -> Grid
intersectTilesEast = intersectTiles gridToEast leftConnector

intersectTilesNorth :: Width -> Length -> Cell -> Grid -> Grid
intersectTilesNorth = intersectTiles gridToNorth downConnector

intersectTilesSouth :: Width -> Length -> Cell -> Grid -> Grid
intersectTilesSouth = intersectTiles gridToSouth upConnector

gridToEntropy :: Width -> Length -> Grid -> Int
gridToEntropy w l g = cellEntropy $ gridToCell w l g

gridToEntropyCell :: Cell -> Grid -> Int
gridToEntropyCell (x, y) = gridToEntropy x y

initialGrid :: Width -> Length -> V.Vector Tile -> Grid
initialGrid w l ts = V.replicate w $ V.replicate l $ CellInfo ts (V.length ts - 1)

-- Returns an IO Tuple of the index of the grid
chooseCell :: Width -> Length -> IO Cell
chooseCell w l = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, l - 1)
  return (x,y)

-- Choose a random tile from the number of available tiles
chooseTile :: Int -> IO Int
chooseTile n = randomRIO (0, n)

-- Takes the tile to pick, the cell to pick within, and the grid to updata.
updateGrid :: Int -> Cell -> Grid -> Grid
updateGrid n (x,y) g = g V.// [(x, newRow)]
  where newTile = V.singleton $ gridToTile x y n g
        newCell = CellInfo newTile 0
        newRow = (g V.! x) V.// [(y, newCell)]

-- Takes a cell and a grid and collapses the cell, creating a new grid
updateTile :: Cell -> Grid -> IO Grid
updateTile (x,y) grid = do
  let numOfTiles = V.length $ gridToTiles x y grid
  tileNum <- randomRIO (0, numOfTiles - 1)
  -- DELETE
  putStrLn "4"
  let newTile = V.singleton $ gridToTile x y tileNum grid
      newCell = CellInfo newTile 0
      newRow = (grid V.! x) V.// [(y, newCell)]
      newGrid = grid V.// [(x, newRow)]
  return newGrid


-- I need to find the minimum non-zero entropy. This function helps
myEntropyOrder :: CellInfo -> CellInfo -> Ordering
myEntropyOrder (CellInfo _ 0) _ = GT
myEntropyOrder _ (CellInfo _ 0) = LT
myEntropyOrder (CellInfo _ a) (CellInfo _ b) | a < b = LT
                                             | a == b = EQ
                                             | a > b = GT

-- Finds the minimum entroy in row
rowMin :: Row -> Int
rowMin = cellEntropy . V.minimumBy myEntropyOrder

-- Finds the minimum entroy in a grid
-- Ensure this is not 0. If is 0 then done
minEntropy :: Grid -> Int
minEntropy g = minimum $ go g (V.length g)
  where go :: Grid -> Int -> [Int]
        go g 0 = [rowMin $ g V.! 0]
        go g n = rowMin (g V.! n) : go g (n-1)

entropyIsCellInfoElem :: Int -> CellInfo -> Bool
entropyIsCellInfoElem e c = e == cellEntropy c

-- Finds the indices with a certain entropy in row
rowEntropyIndices :: Int -> Row -> V.Vector Int
rowEntropyIndices e = V.findIndices (entropyIsCellInfoElem e)

-- Finds the indices with a certain entropy in grid.
-- Use randomRIO to choose one of these cells
entropyIndices :: Int -> Grid -> V.Vector Cell
entropyIndices e g = go e g (V.length g)
  where f :: Int -> Int -> (Int, Int)
        f n i = (n,i)
        helper :: Int -> Grid -> Int -> V.Vector Cell
        helper e g n = V.map (f n) (rowEntropyIndices e (g V.! n))
        go :: Int -> Grid -> Int -> V.Vector Cell
        go e g 0 = helper e g 0
        go e g n = helper e g n V.++ go e g (n - 1)

-- Takes the last cell that was updated and the grid itslef
propagate :: Cell -> Grid -> Grid
propagate c g = go [c] (V.length g) (V.length (g V.! 0)) g
  where
    north x y l = [(x, y + 1) | (y + 1) <= l]
    east  x y w = [(x + 1, y) | (x + 1) <= w]
    south x y = [(x, y - 1) | (y - 1) >= 0]
    west  x y = [(x - 1, y) | (x - 1) >= 0]
    getNeighbors :: Cell -> Width -> Length -> [Cell]
    getNeighbors (x,y) w l = north x y l ++ east x y l ++ south x y ++ west x y
    validateNeighbors :: [Cell] -> Grid -> [Cell]
    validateNeighbors [] _ = []
    validateNeighbors (c:cs) g = case gridToEntropyCell c g of
                                   0 -> validateNeighbors cs g
                                   _ -> c : validateNeighbors cs g
    neighbors :: Cell -> Width -> Length -> Grid -> [Cell]
    neighbors c w l = validateNeighbors (getNeighbors c w l)
    collapseNorth x y w l g = case north x y l of
                           [] -> collapseEast x y w g
                           [c] -> case gridToEntropyCell c g of
                                    0 -> collapseEast x y w g
                                    _ -> collapseEast x y w $ intersectTilesNorth x y c g
                           _ -> throw $ PatternMatchFail "List has too many elements."
    collapseEast x y w g = case east x y w of
                             [] -> collapseSouth x y g
                             [c] -> case gridToEntropyCell c g of
                                      0 -> collapseSouth x y g
                                      _ -> collapseSouth x y $ intersectTilesEast x y c g
                             _ -> throw $ PatternMatchFail "List has too many elements."
    collapseSouth x y g = case south x y  of
                            [] -> collapseWest x y g
                            [c] -> case gridToEntropyCell c g of
                                     0 -> collapseWest x y g
                                     _ -> collapseWest x y $ intersectTilesSouth x y c g
                            _ -> throw $ PatternMatchFail "List has too many elements."
    collapseWest x y g = case west x y  of
                           [] -> g
                           [c] -> case gridToEntropyCell c g of
                                     0 -> g
                                     _ -> intersectTilesWest x y c g
                           _ -> throw $ PatternMatchFail "List has to many elements."
    collapse :: Width -> Length -> Width -> Length -> Grid -> Grid
    collapse = collapseNorth
    go :: [Cell] -> Width -> Length -> Grid -> Grid
    go [] w l g = undefined
    go ((x,y):cs) w l g = go (neighbors (x,y) w l g ++ cs) w l $ collapse x y w l g

-- A method to choose the first tile.
-- Eventually add alternative methods that allow user
-- to choose the first tile.
firstCollapse :: IO Grid
firstCollapse = do
  myPutStr "Horizontal number of tiles (width): "
  w <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  l <- intGetLine
  tiles <- getAllTiles
  let grid = initialGrid w l tiles
  cell <- chooseCell w l
  n <- chooseTile $ length tiles
  return $ propagate cell $ updateGrid n cell grid

-- THE WAVE FUNCTION!!!!!
-- Repeat until all entropy is 0 or failure.
wave :: Grid -> IO Grid
wave grid = do
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
      -- DELETE
      putStrLn "1"
      let cell = indices V.! index
      -- Collapse the cell and create a new grid
      newGrid <- updateTile cell grid
      -- Propagate the wave through the new grid, updating all cells
      wave $ propagate cell newGrid

-- CALL THIS
waveMain :: IO ()
waveMain = do
  grid <- firstCollapse
  finalGrid <- wave grid
  printGrid finalGrid

