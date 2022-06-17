{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Tiles where

import Utils

import Control.Applicative ()
import Data.Text ( Text )
import Text.RawString.QQ ( r )
import Data.Typeable ( Typeable )

import Database.SQLite.Simple
    ( close,
      execute,
      execute_,
      open,
      query,
      query_,
      field,
      Only(Only),
      FromRow(..),
      Connection,
      ToRow(..),
      Query )
import Database.SQLite.Simple.Types ( Null(..) )
import Control.Exception (Exception, throwIO)

data Tile = Tile {
    tileId :: Int
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
   filePath TEXT UNIQUE,
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

data Rotation = None | Once | Twice | Thrice

createTileRow :: Text -> Text -> Rotation -> Int -> Int -> Int -> Int -> Int -> [TileRow]
createTileRow name path rot l r u d w =
  case rot of
    None   -> [ (Null, name, path, 0, l, r, u, d, w) ]
    Once   -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, name, path, 90, l, r, u, d, w) ]
    Twice  -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, name, path, 90, l, r, u, d, w)
              , (Null, name, path, 180, l, r, u, d, w) ]
    Thrice -> [ (Null, name, path, 0, l, r, u, d, w)
              , (Null, name, path, 90, l, r, u, d, w)
              , (Null, name, path, 180, l, r, u, d, w)
              , (Null, name, path, 270, l, r, u, d, w) ]

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
  conn <- open "test.db"
  execute_ conn createTiles
  execute conn insertTile myRow
  rows <- query_ conn allTiles
  mapM_ print (rows :: [Tile])
  close conn
  where myRow :: TileRow
        myRow = (Null, "Test Tile", "not a real path", 0, 1, 1, 1, 1, 10)
