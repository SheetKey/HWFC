{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Tiles where

import Control.Applicative
import Data.Text
import Text.RawString.QQ
import Data.Typeable

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Types
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

insertTile :: Query
insertTile = "INSERT INTO users\
             \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

allTiles :: Query
allTiles = "SELECT * from tiles"

getTileQuery :: Query
getTileQuery = "SELECT * from tiles where name = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type TileRow = (Null, Text, Text, Int, Int, Int, Int, Int, Int)

getTile :: Connection -> Text -> IO (Maybe Tile)
getTile conn name = do
  results <- query conn getTileQuery (Only name)
  case results of
    [] -> return Nothing
    [tile] -> return $ Just tile
    _ -> throwIO DuplicateData

creatDatabase :: IO ()
creatDatabase = do
  conn <- open "test.db"
  execute_ conn createTiles
  execute conn insertTile meRow
  rows <- query_ conn allTiles
  mapM_ print (rows :: [Tile])
  close conn
  where meRow :: TileRow
        meRow = (Null, "Test Tile", "not a real path", 0, 1, 1, 1, 1, 10)
