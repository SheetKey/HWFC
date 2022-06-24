{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Tiles where

import Utils ( myPutStr, intGetLine, textGetLine )

import Control.Applicative ()
import Data.Text ( Text, append )
import Text.RawString.QQ ( r )
import Data.Typeable ( Typeable )

import Database.SQLite.Simple
    ( close,
      execute,
      execute_,
      open,
      query,
      field,
      Only(Only),
      FromRow(..),
      Connection (Connection),
      ToRow(..),
      Query, fold_ )
import Database.SQLite.Simple.Types ( Null(..) )
import Control.Exception (Exception, throwIO)
import qualified Data.Vector as V

type TileId = Int

data Tile = Tile {
    tileId :: TileId
  , tileName :: Text
  , tileFilePath :: Text
  , tileRotation :: Int
  , leftConnector :: Int
  , rightConnector :: Int
  , upConnector :: Int
  , downConnector :: Int
  , tileWeight :: Int
  } deriving (Eq, Show)

instance FromRow Tile where
  fromRow = Tile <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
  
instance ToRow Tile where
  toRow (Tile id_ name path rot l r u d w) = toRow (id_, name, path, rot, l, r, u, d, w)

createTiles :: Query
createTiles = [r|
CREATE TABLE IF NOT EXISTS tiles
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   name TEXT UNIQUE,
   filePath TEXT,
   tileRotation INTEGER,
   leftConnector INTEGER,
   rightConnector INTEGER,
   upConnector INTEGER,
   downConnector INTEGER,
   tileWeight INTEGER)
|]

-- Insert a new tile
insertTile :: Query
insertTile = "INSERT INTO tiles VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

-- Get all the tiles
allTiles :: Query
allTiles = "SELECT * from tiles"

-- Get all fields from a tile with a certain name
getTileQuery :: Query
getTileQuery = "SELECT * from tiles where name = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type TileRow = (Null, Text, Text, Int, Int, Int, Int, Int, Int)

-- Gets a possible tile from a tile name
-- This is an example. Not something Ill use probably
getTile :: Connection -> Text -> IO (Maybe Tile)
getTile conn name = do
  results <- query conn getTileQuery (Only name)
  case results of
    [] -> return Nothing
    [tile] -> return $ Just tile
    _ -> throwIO DuplicateData

-- A type to deal with how many times a tile should be rotated
data Rotation = None | Once | Twice | Thrice

-- Creates a list of TileRows that can be used to create tiles
--  ***NOTE***
-- THIS FUNCTION TAKES CARE OF ROTATION OF CONNECTORS
-- BUT NOT OF FILE ROTATION AS IT ONLY INCLUDES A PATH TO THE FILE
createTileRow :: Text -> Text -> Rotation -> Int -> Int -> Int -> Int -> Int -> [TileRow]
createTileRow name path rot l r u d w =
  case rot of
    None   -> [ (Null, name, path, 0, l, r, u, d, w) ]
    Once   -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, append name "+90", path, 90, u, d, r, l, w) ]
    Twice  -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, append name "+90", path, 90, u, d, r, l, w)
              , (Null, append name "+180", path, 180, r, l, d, u, w) ]
    Thrice -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, append name "+90", path, 90, u, d, r, l, w)
              , (Null, append name "+180", path, 180, r, l, d, u, w)
              , (Null, append name "+270", path, 270, d, u, l, r, w) ]

-- A helper function to get how many times a tile should be rotated
rotGetLine :: IO Rotation
rotGetLine = do
  n <- intGetLine
  case n of
    0 -> return None
    1 -> return Once
    2 -> return Twice
    3 -> return Thrice
    _ -> do
      putStrLn "Invalid input."
      rotGetLine

-- A function to insert a new tile to a given connection 
getInsertTile :: Connection -> IO ()
getInsertTile conn = do
  myPutStr "Tile name: "
  name <- textGetLine
  myPutStr "Path to tile file: "
  path <- textGetLine
  putStrLn "How may times should tile be rotated?"
  myPutStr "0, 1, 2, or 3: "
  rot <- rotGetLine
  myPutStr "Left connector: "
  l <- intGetLine
  myPutStr "Right connector: "
  r <- intGetLine
  myPutStr "Up connector: "
  u <- intGetLine
  myPutStr "Down connector: "
  d <- intGetLine
  myPutStr "Tile weight: "
  w <- intGetLine
  foldMap (execute conn insertTile) $ createTileRow name path rot l r u d w

creatDatabase :: IO ()
creatDatabase = do
  conn <- open "Tiles/test.db"
  execute_ conn createTiles
  foldMap (execute conn insertTile) $ createTileRow "Test1"
                                                    "./Tiles/Test1.png"
                                                    Once 0 0 1 1 1
  foldMap (execute conn insertTile) $ createTileRow "Test2"
                                                    "./Tiles/Test2.png"
                                                    Thrice 0 0 1 0 1
  rows <- rowToVectorIO conn
  mapM_ print (rows :: V.Vector Tile)
  close conn

createNewTile :: IO ()
createNewTile = do
  conn <- open "Tiles/test.db"
  getInsertTile conn
  mapM_ print (rows :: V.Vector Tile)
  close conn

-- A function to use with fold_ for SQLite queries
rowToVector :: V.Vector Tile -> Tile -> IO (V.Vector Tile)
rowToVector v t = return $ V.cons t v

rowToVectorIO :: Connection -> IO (V.Vector Tile)
rowToVectorIO conn = fold_ conn allTiles V.empty rowToVector

getAllTiles :: IO (V.Vector Tile)
getAllTiles = do
  conn <- open "Tiles/test.db"
  rows <- rowToVectorIO conn
  mapM_ print (rows :: V.Vector Tile)
  close conn
  return rows

