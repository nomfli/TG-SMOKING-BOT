module DB (TableUser, TableFriends, createDB, addUser, getUser, addFriend) where
import Prelude hiding (id)
import Database.HDBC (run, commit, disconnect, toSql, fromSql, quickQuery')
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Data.Text                        as Text
import Data.Text (Text)

data TableUser = TableUser
    { userid    :: Int
    , username  :: Text
    }

data TableFriends = TableFriends
    { strongId :: Int
    , weakId :: Int
    }

-- toPair :: (Maybe a, Maybe b) -> Maybe (a, b)
-- toPair (Just y, Just z) = Just (y, z)
-- toPair _                = Nothing

-- logMsg :: MonadIO m => String -> m ()
-- logMsg msg = liftIO $ putStrLn msg


createDB :: IO()
createDB = do
  conn <- connectSqlite3 "Users.db"
  _ <- run conn "CREATE TABLE IF NOT EXISTS names (ID INTEGER PRIMARY KEY AUTOINCREMENT, tgusername TEXT UNIQUE)" []
  _ <- run conn ("CREATE TABLE IF NOT EXISTS Friends (" ++
           "ID INTEGER PRIMARY KEY AUTOINCREMENT, " ++
           "WeakID INTEGER, " ++
           "StrongID INTEGER, " ++
           "FOREIGN KEY (WeakID) REFERENCES names(tgusername), " ++
           "FOREIGN KEY (StrongID) REFERENCES names(tgusername))") []
  _ <- run conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_unique_friendship ON Friends (WeakID, StrongID)" []
  commit conn
  disconnect conn

-- it's my thing about how addUser should looks in the future probably
-- addUser :: Text -> WriterT () IO ()
-- addUser tgusername = do
--   conn <- liftIO $ connectSqlite3 "Users.db"
--    let query = "INSERT INTO tgusername (tgusername) VALUES (?)"
--    liftIO $ catch (run conn query [toSql tgusername] >> commit conn >> putStrLn successMsg)
--                   (\e -> putStrLn $ "Failed to add user to db: " ++ 
--                   Text.unpack tgusername ++ 
--                   "\nSQL Error: " ++ 
--                   show (e :: SomeException))
--    liftIO $ disconnect conn
--  where
--    successMsg = "Successfully added user to db: " ++ Text.unpack tgusername



addUser ::  Text -> IO (Maybe TableUser)
addUser tgusername = do
    conn <- connectSqlite3 "Users.db"
    
    let query = "INSERT OR IGNORE INTO names (tgusername) VALUES (?)"
    _ <- run conn query [toSql tgusername]
    commit conn
    disconnect conn
    putStrLn $ "Attempted to add user " ++ Text.unpack tgusername ++ " to DB"
    getUser tgusername



getUser :: Text -> IO (Maybe TableUser)
getUser un = do
    conn <- connectSqlite3 "Users.db"
    let query = "SELECT id, tgusername FROM names WHERE tgusername = ?"
    rows <- quickQuery' conn query [toSql $ Text.unpack un]
    disconnect conn
    case rows of
      [row] -> return $ Just TableUser { userid = fromSql $ head row
                                       , username = fromSql $ row !! 1
                                       }
      _     -> return Nothing



addFriend :: TableUser -> TableUser -> IO (Maybe TableFriends)
addFriend user friend = do
    conn <- connectSqlite3 "Users.db"
    let usrId = userid user
    let frndId = userid friend
    let query = "INSERT OR IGNORE INTO Friends (StrongId, WeakId) VALUES (?,?)"
    _ <- run conn query [toSql usrId, toSql frndId]
    commit conn
    putStrLn "Friend added successfully"
    disconnect conn
    return $ Just TableFriends { strongId = usrId, weakId = frndId }
