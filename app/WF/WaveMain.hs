module WF.WaveMain where

import Database.Tiles (getAllTiles, Tile (downConnector, tileId))
import WF2.Wave
import Utils (myPutStr, intGetLine)
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Exception (AssertionFailed(AssertionFailed), throw)

startWave :: IO (Grid,Entropy,Adjacency)
startWave = do
  myPutStr "Horizontal number of tiles (width): "
  xmax <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  ymax <- intGetLine
  tiles <- getAllTiles
  let fstGrid = createGrid tiles xmax ymax
      fstEntr = createEntropy tiles xmax ymax
      adjacencies = createAdjacency tiles
      fstEmin = entropyMin fstEntr
  case fstEmin of
    Nothing -> throw $ AssertionFailed "No min entropy"
    Just (cell,_) -> do
      (sndGrid,sndEntr) <- updateGridandEntropy cell fstGrid fstEntr
      let (thdGrid,thdEntr) = propagate adjacencies cell sndEntr sndGrid
      return (thdGrid,thdEntr,adjacencies)


wave :: (Grid,Entropy,Adjacency) -> IO Grid
wave (grid,entr,adja) = do
  if M.null entr
    then do
    putStrLn "Finished!"
    return grid
    else do
    let emin = entropyMin entr
    case emin of
      Nothing -> throw $ AssertionFailed "No min entropy"
      Just (cell,_) -> do
        (sndGrid,sndEntr) <- updateGridandEntropy cell grid entr
        let (thdGrid,thdEntr) = propagate adja cell sndEntr sndGrid
        wave (thdGrid,thdEntr,adja)
        
waveMain :: IO ()
waveMain = do
  start <- startWave
  grid <- wave start
  print $ (fmap . fmap) tileId grid


t = do
  myPutStr "Horizontal number of tiles (width): "
  xmax <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  ymax <- intGetLine
  tiles <- getAllTiles
  let fstGrid = createGrid tiles xmax ymax
      fstEntr = createEntropy tiles xmax ymax
      fstAdja = createAdjacency tiles
      fstEmin = entropyMin fstEntr
  case fstEmin of
    Nothing -> putStrLn "firstEmin found Nothing"
    Just (cell,_) -> do
      (sndGrid,sndEntr) <- updateGridandEntropy cell fstGrid fstEntr
      let (thdGrid,thdEntr) = propagate fstAdja cell sndEntr sndGrid
      print thdGrid
      print thdEntr

test = do
  myPutStr "Horizontal number of tiles (width): "
  xmax <- intGetLine
  myPutStr "Verticle number of tiles (length): "
  ymax <- intGetLine
  tiles <- getAllTiles
  let fstGrid = createGrid tiles xmax ymax
      fstEntr = createEntropy tiles xmax ymax
      fstAdja = createAdjacency tiles
      fstEmin = entropyMin fstEntr
  case fstEmin of
    Nothing -> putStrLn "firstEmin found Nothing"
    Just (cell,_) -> do
      (sndGrid,sndEntr) <- updateGridandEntropy cell fstGrid fstEntr
      case north cell sndEntr of
        Nothing -> putStrLn "Found nothing to north"
        Just x  ->
          case sndGrid M.! x of
            Right _ -> putStrLn "north found collapsed cell"
            Left tileses ->
              let adj = cellToAdjacencies cell sndGrid North fstAdja
              in if vecIsSubset tileses adj
                 then putStrLn "vec is subset"
                 else print $ intersectTilesWithAdj adj x sndEntr sndGrid


