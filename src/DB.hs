module DB where

import Control.Monad.IO.Class (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text


createDB :: IO()
createDB = do
  conn <- connectSqlite3 "names.db"
  run conn "CREATE TABLE IF NOT EXISTS tgId (ID INTEGER PRIMARY KEY AUTOINCREMENT, tgID TEXT UNIQUE)" []
  commit conn
  disconnect conn

  fconn <- connectSqlite3 "friends.db"
  run fconn ("CREATE TABLE IF NOT EXISTS Friends (" ++
           "ID INTEGER PRIMARY KEY AUTOINCREMENT, " ++
           "WeakID Text, " ++
           "StrongID Text, " ++
           "FOREIGN KEY (WeakID) REFERENCES names(tgID), " ++
           "FOREIGN KEY (StrongID) REFERENCES names(tgID))") []

  run fconn "CREATE UNIQUE INDEX IF NOT EXISTS idx_unique_friendship ON Friends (WeakID, StrongID)" []
  
  commit fconn
  disconnect fconn



addUser :: Connection -> Text -> IO ()
addUser conn tgID = do
  let query = "INSERT OR IGNORE INTO tgId (tgID) VALUES (?)"
  run conn query [toSql tgID]
  commit conn
  putStrLn $ "Attempted to add user to db: " ++ Text.unpack tgID



addFriend :: Connection -> Text -> Text -> IO ()
addFriend conn username (friendname) = do
  let query = "INSERT OR IGNORE INTO Friends (WeakID, StrongID) VALUES (?, ?)"
  run conn query [toSql username, toSql (Text.tail friendname)]
  commit conn
  putStrLn $ "Attempted to add friend: " ++ Text.unpack username ++ " -> " ++ Text.unpack friendname


