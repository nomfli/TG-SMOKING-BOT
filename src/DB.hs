module DB where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.HDBC (run, commit, disconnect, toSql, fromSql, quickQuery')
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import qualified Data.Text                        as Text
import Data.Text (Text)
import Control.Exception (catch, SomeException)
import Control.Monad.Trans.Writer (WriterT)


logMsg :: MonadIO m => String -> m ()
logMsg msg = liftIO $ putStrLn msg


createDB :: IO()
createDB = do
  conn <- connectSqlite3 "Users.db"
  run conn "CREATE TABLE IF NOT EXISTS names (ID INTEGER PRIMARY KEY AUTOINCREMENT, tgID TEXT UNIQUE)" []
  run conn ("CREATE TABLE IF NOT EXISTS Friends (" ++
           "ID INTEGER PRIMARY KEY AUTOINCREMENT, " ++
           "WeakID Text, " ++
           "StrongID Text, " ++
           "FOREIGN KEY (WeakID) REFERENCES names(tgID), " ++
           "FOREIGN KEY (StrongID) REFERENCES names(tgID))") []
  run conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_unique_friendship ON Friends (WeakID, StrongID)" []
  commit conn
  disconnect conn

-- it's my thing about how addUser should looks in the future probably
-- addUser :: Text -> WriterT () IO ()
-- addUser tgID = do
--   conn <- liftIO $ connectSqlite3 "Users.db"
--    let query = "INSERT INTO tgId (tgID) VALUES (?)"
--    liftIO $ catch (run conn query [toSql tgID] >> commit conn >> putStrLn successMsg)
--                   (\e -> putStrLn $ "Failed to add user to db: " ++ 
--                   Text.unpack tgID ++ 
--                   "\nSQL Error: " ++ 
--                   show (e :: SomeException))
--    liftIO $ disconnect conn
--  where
--    successMsg = "Successfully added user to db: " ++ Text.unpack tgID



addUser ::  Text -> IO ()
addUser tgID = do
    conn <- connectSqlite3 "Users.db"
    let query = "INSERT OR IGNORE INTO names (tgID) VALUES (?)"
    run conn query [toSql tgID]
    commit conn
    disconnect conn
    putStrLn $ "Attempted to add user " ++ Text.unpack tgID ++ " to DB"

userExists :: Text -> IO Bool
userExists username = do
    conn <- connectSqlite3 "Users.db"
    result <- quickQuery' conn "SELECT COUNT(*) FROM names WHERE tgID = ?" [toSql username]
    disconnect conn
    let count = fromSql (head (head result)) :: Integer
    return (count /= 0)


addFriend :: (Text, Text) -> IO ()
addFriend (username, friendname) = do
    exists1 <- userExists username
    exists2 <- userExists friendname
    if exists1 && exists2
        then do
            conn <- connectSqlite3 "Users.db"
            run conn "INSERT OR IGNORE INTO Friends (WeakID, StrongID) VALUES (?, ?)" [toSql username, toSql friendname]
            commit conn
            putStrLn $ "Successfully added friend: " ++ Text.unpack username ++ " -> " ++ Text.unpack friendname
            disconnect conn
        else
            putStrLn $ "One or both users not found: " ++ Text.unpack username ++ ", " ++ Text.unpack friendname



getFriends :: Connection -> Text -> IO [Text] 
getFriends conn username = do
  conn <- connectSqlite3 "Users.db"
  let usernameStr = Text.unpack username
  rows <- quickQuery' conn "SELECT StrongID FROM Friends WHERE WeakID = ?" [toSql usernameStr]
  disconnect conn
  return $ map (Text.pack . fromSql . head) rows
