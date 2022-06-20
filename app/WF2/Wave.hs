module WF2.Wave where

import Database.Tiles
    ( Tile(downConnector, upConnector, leftConnector, rightConnector,
           tileId),
      TileId )


import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Exception (throw, ArrayException (IndexOutOfBounds), AssertionFailed (AssertionFailed))
import System.Random (randomRIO)

type XMax = Int
type YMax = Int
type X = Int
type Y = Int
type Cell = (X, Y)
data Direction = North | East | South | West deriving (Eq, Ord, Show)

type GridMap a = M.Map Cell a

type Grid = GridMap (Either (V.Vector Tile) Tile)
type Entropy = GridMap Int
type Adjacency = M.Map (TileId, Direction) (V.Vector Tile)

createGrid :: V.Vector Tile -> XMax -> YMax -> Grid
createGrid tiles xMax yMax = M.fromList
  [ ((x,y),Left tiles) | x <- [0..(xMax - 1)], y <- [0..(yMax - 1)] ]

createEntropy :: V.Vector Tile -> XMax -> YMax -> Entropy
createEntropy tiles xMax yMax = M.fromList
  [ ((x,y), V.length tiles - 1) | x <- [0..(xMax - 1)], y <- [0..(yMax - 1)] ]

createAdjacency :: V.Vector Tile -> Adjacency
createAdjacency tiles = M.fromList
                        [ ((tileId tile,dir), adjVec tile dir (V.length tiles - 1))
                        | tile <- V.toList tiles, dir <- [North,East,South,West]
                        ]
  where adjVec :: Tile -> Direction -> Int -> V.Vector Tile
        adjVec aTile aDir 0 = case aDir of
                                North -> if upConnector aTile == downConnector (tiles V.! 0)
                                         then V.singleton $ tiles V.! 0
                                         else V.empty
                                East  -> if rightConnector aTile == leftConnector (tiles V.! 0)
                                         then V.singleton $ tiles V.! 0
                                         else V.empty
                                South -> if downConnector aTile == upConnector (tiles V.! 0)
                                         then V.singleton $ tiles V.! 0
                                         else V.empty
                                West  -> if leftConnector aTile == rightConnector (tiles V.! 0)
                                         then V.singleton $ tiles V.! 0
                                         else V.empty
        adjVec aTile aDir n = case aDir of
                                North -> if upConnector aTile == downConnector (tiles V.! n)
                                         then tiles V.! n `V.cons` adjVec aTile aDir (n - 1)
                                         else adjVec aTile aDir (n - 1)
                                East  -> if rightConnector aTile == leftConnector (tiles V.! n)
                                         then tiles V.! n `V.cons` adjVec aTile aDir (n - 1)
                                         else adjVec aTile aDir (n - 1)
                                South -> if downConnector aTile == upConnector (tiles V.! n)
                                         then tiles V.! n `V.cons` adjVec aTile aDir (n - 1)
                                         else adjVec aTile aDir (n - 1)
                                West  -> if leftConnector aTile == rightConnector (tiles V.! n)
                                         then tiles V.! n `V.cons` adjVec aTile aDir (n - 1)
                                         else adjVec aTile aDir (n - 1)

-- Should be nothing if and only if all entropy is empty.
-- Requires random numbers if there are multiple cells with the same entropy
entropyMinCells :: Entropy -> Maybe ([Cell], Int)
entropyMinCells = M.foldrWithKey
                  (\k a b -> case a of
                               0 -> throw $ AssertionFailed "Zero entropy found."
                               _ -> case b of
                                      Nothing       -> Just ([k], a)
                                      Just (bk, be) -> case compare a be of
                                                         GT -> b
                                                         EQ -> Just (k:bk, a)
                                                         LT -> Just ([k], a)
                  )
                  Nothing

-- Should be nothing if and only if all entropy is empty.
-- Doesnt require use of random numbers if there are multiple cells with the same entroy
entropyMin :: Entropy -> Maybe (Cell, Int)
entropyMin = M.foldrWithKey
             (\k a b -> case a of
                          0 -> throw $ AssertionFailed "Zero entropy found."
                          _ -> case b of
                                 Nothing       -> Just (k, a)
                                 Just (_, be) -> case compare a be of
                                                   GT -> b
                                                   EQ -> Just (k, a)
                                                   LT -> Just (k, a)
             )
             Nothing

-- Takes a cell, most likely found from the 'entropyMin' or 'entropyMinCells' functions,
-- and updates the grid at that cell
-- and deletes the key from the entropy Map.
updateGridandEntropy :: Cell -> Grid -> Entropy -> IO (Grid, Entropy)
updateGridandEntropy c g e =
  case (M.lookup c g, M.lookup c e) of
    (Nothing, _)         -> throw $ AssertionFailed "Cell not a part of grid."
    (_, Nothing)         -> throw $ AssertionFailed "Cell not a part of entropy."
    (Just eitherTile, _) -> case eitherTile of
                              Right _    -> throw $ AssertionFailed "Cell already collapsed."
                              Left tiles -> do
                                let num = V.length tiles - 1
                                choice <- randomRIO (0, num)
                                let newGrid = M.insert c (Right (tiles V.! choice)) g
                                    newEntropy = M.delete c e
                                return (newGrid, newEntropy)

checkDir :: (Cell -> Cell) -> Cell -> Entropy -> Maybe Cell
checkDir f c e = if M.member (f c) e then Just (f c) else Nothing

north :: Cell -> Entropy -> Maybe Cell
north = checkDir (\(x,y) -> (x + 1,y))

east :: Cell -> Entropy -> Maybe Cell
east = checkDir (\(x,y) -> (x,y + 1))

south :: Cell -> Entropy -> Maybe Cell
south = checkDir (\(x,y) -> (x - 1,y))

west :: Cell -> Entropy -> Maybe Cell
west = checkDir (\(x,y) -> (x,y - 1))


intersectVecs :: Eq a => V.Vector a -> V.Vector a -> V.Vector a
intersectVecs v1 v2 = if V.length v1 <= V.length v2
                      then go v1 v2 $ V.length v1 - 1
                      else go v2 v1 $ V.length v2 - 1
        -- go: Is called with the shorter vector first and the length of the shorter
        -- vector - 1.
  where go :: Eq a => V.Vector a -> V.Vector a -> Int -> V.Vector a
        go vs vl 0 = if (vs V.! 0) `V.elem` vl
                     then V.singleton $ vs V.! 0
                     else V.empty
        go vs vl n = if (vs V.! n) `V.elem` vl
                     then (vs V.! n) `V.cons` go vs vl (n - 1)
                     else go vs vl (n - 1)

-- Gets the adjacencies of a tile
getAdjacency :: Tile -> Direction -> Adjacency -> V.Vector Tile
getAdjacency t dir a = a M.! (tileId t, dir)

-- Gets the adjacencies of many tiles
getAdjacencies :: V.Vector Tile -> Direction -> Adjacency -> V.Vector Tile
getAdjacencies tiles dir a = V.uniq $ go (V.length tiles - 1) tiles dir a
  where go :: Int -> V.Vector Tile -> Direction -> Adjacency -> V.Vector Tile
        go 0 ts d a = getAdjacency (ts V.! 0) d a
        go n ts d a = getAdjacency (ts V.! n) d a V.++ go (n - 1) ts d a

-- THIS IS UNSAFE. SHOULD ONLY BE CALLED BY 'propagate' WHICH SHOULD
-- GUARUNTEE SAFETY!!!
intersectTiles :: Direction -> Cell -> Cell -> Adjacency -> Entropy -> Grid -> (Grid, Entropy)
intersectTiles d c x a e g =
  case g M.! c of
    Right tileC -> case g M.! x of
                    Right tileX ->
                      throw $ AssertionFailed
                      "'intersectTiles' called on a tile that has already been collapsed."
                    Left tilesX ->
                       let adj = getAdjacency tileC d a
                           inter = intersectVecs adj tilesX
                       --in if V.null inter
                       --then throw $ AssertionFailed "No tile adjacencies."
                       --else (M.insert x (Left inter) g, M.insert x (V.length inter - 1) e)
                       in case V.length inter of
                            0 -> throw $ AssertionFailed "No tile adjacencies."
                            1 -> (M.insert x (Right (inter V.! 0)) g, M.delete x e)
                            _ -> (M.insert x (Left inter) g, M.insert x (V.length inter - 1) e)
    Left tilesC -> case g M.! x of
                     Right tileX ->
                       throw $ AssertionFailed
                       "'intersectTiles' called on a tile that has already been collapsed."
                     Left tilesX ->
                       let adj = getAdjacencies tilesC d a
                           inter = intersectVecs adj tilesX
                       --in if V.null inter
                       --then throw $ AssertionFailed "No tile adjacencies."
                       --else (M.insert x (Left inter) g, M.insert x (V.length inter - 1) e)
                       in case V.length inter of
                            0 -> throw $ AssertionFailed "No tile adjacencies."
                            1 -> (M.insert x (Right (inter V.! 0)) g, M.delete x e)
                            _ -> (M.insert x (Left inter) g, M.insert x (V.length inter - 1) e)



propagate :: Adjacency -> Cell -> Entropy -> Grid -> (Grid, Entropy)
propagate a cell = go North [cell]
  where go :: Direction -> [Cell] -> Entropy -> Grid -> (Grid, Entropy)
        go _ [] e g         = (g, e)
        go North (c:cs) e g = case north c e of
                                Nothing -> go East (c:cs) e g
                                Just x  ->
                                  let (newGrid,newEntr) = intersectTiles North c x a e g
                                  in go East (c:x:cs) newEntr newGrid
        go East (c:cs) e g = case east c e of
                               Nothing -> go South (c:cs) e g
                               Just x ->
                                 let (newGrid,newEntr) = intersectTiles East c x a e g
                                 in go South (c:x:cs) newEntr newGrid
        go South (c:cs) e g = case south c e of
                                Nothing -> go West (c:cs) e g
                                Just x ->
                                  let (newGrid,newEntr) = intersectTiles South c x a e g
                                  in go West (c:x:cs) newEntr newGrid
        go West (c:cs) e g = case west c e of
                               Nothing -> go North cs e g
                               Just x ->
                                 let (newGrid,newEntr) = intersectTiles West c x a e g
                                 in go North (x:cs) newEntr newGrid

