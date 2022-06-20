module WF.Propagate where

import Database.Tiles ( Tile (downConnector, upConnector, rightConnector, leftConnector, tileId), getAllTiles )
import WF.Wave
    ( Width,
      Length,
      Cell,
      Grid,
      gridToEntropyCell,
      intersectTilesWest,
      intersectTilesEast,
      intersectTilesNorth,
      intersectTilesSouth, printGrid )
import Utils (myPutStr, intGetLine)

import Control.Exception (throw, PatternMatchFail (PatternMatchFail), AssertionFailed (AssertionFailed))

import qualified Data.Vector as V
import Control.Applicative (liftA3)
import Data.Maybe (fromMaybe)

north :: Width -> Length -> Length -> Maybe Cell
north x y l = if y + 1 < l then Just (x, y + 1) else Nothing

east :: Width -> Length -> Width -> Maybe Cell
east x y w = if x + 1 < w then Just (x + 1, y) else Nothing

south :: Width -> Length -> Maybe Cell
south x y = if y - 1 >= 0 then Just (x, y - 1) else Nothing

west :: Width -> Length -> Maybe Cell
west x y = if x - 1 >= 0 then Just (x - 1, y) else Nothing

getNeighbors :: Cell -> Width -> Length -> Maybe [Cell]
getNeighbors (x,y) w l =
  (\a b c d -> [a, b, c, d]) <$> north x y l <*> east x y l <*> south x y <*> west x y

validateNeighbors :: Maybe [Cell] -> Grid -> Maybe [Cell]
validateNeighbors Nothing _ = Nothing
validateNeighbors (Just []) _ = Just []
validateNeighbors (Just (c:cs)) g = case gridToEntropyCell c g of
                                      0 -> validateNeighbors (Just cs) g
                                      _ -> Just (c:) <*> validateNeighbors (Just cs) g

neighbors :: Cell -> Width -> Length -> Grid -> Maybe [Cell]
neighbors c w l = validateNeighbors (getNeighbors c w l)

collapseNorth :: Width -> Length -> Length -> Grid -> Grid
collapseNorth x y l g = case north x y l of
                          Nothing -> g
                          Just c -> case gridToEntropyCell c g of
                                      0 -> g
                                      _ -> intersectTilesNorth x y c g

collapseEast :: Width -> Length -> Width -> Grid -> Grid
collapseEast x y w g = case east x y w of
                         Nothing -> g
                         Just c -> case gridToEntropyCell c g of
                                     0 -> g
                                     _ -> intersectTilesEast x y c g

collapseSouth :: Width -> Length -> Grid -> Grid
collapseSouth x y g = case south x y  of
                        Nothing -> g
                        Just c -> case gridToEntropyCell c g of
                                    0 -> g
                                    _ -> intersectTilesSouth x y c g

collapseWest :: Width -> Length -> Grid -> Grid
collapseWest x y g = case west x y  of
                       Nothing -> g
                       Just c -> case gridToEntropyCell c g of
                                   0 -> g
                                   _ -> intersectTilesWest x y c g

-- NOTE:
-- x and y correspond to the current cell
-- w and l correspond to the width and length of g
collapse :: Width -> Length -> Width -> Length -> Grid -> Grid
collapse x y w l g = collapseNorth x y l
                   $ collapseEast x y w
                   $ collapseSouth x y
                   $ collapseWest x y g

propagate :: [Cell] -> Width -> Length -> Grid -> Grid
propagate [] _ _ g = g
propagate ((x, y):cs) w l g = propagate newCells w l $ collapse x y w l g
  where newCells = fromMaybe [] (neighbors (x, y) w l g)


debugPropagate :: [Cell] -> Width -> Length -> Grid -> IO Grid
debugPropagate [] _ _ g = return g
debugPropagate ((x, y):cs) w l g = do
  let neighbs = neighbors (x, y) w l g
  myPutStr "neighbs: "
  print neighbs
  let newCells = fromMaybe [] neighbs
  myPutStr "newCells: "
  print newCells
  let newG = collapse x y w l g
  myPutStr "newG: "
  printGrid newG
  debugPropagate newCells w l newG
