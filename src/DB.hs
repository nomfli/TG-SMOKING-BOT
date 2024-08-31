module DB (TableUser, TableFriends, createDB, addUser, getUser, addFriend) where
import Prelude hiding (id)
import Database.HDBC (run, commit, disconnect, toSql, fromSql, quickQuery')
import Control.Monad.IO.Class (liftIO)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Data.Text                        as Text
import Data.Text (Text)
import Control.Monad.Trans.Maybe (MaybeT, hoistMaybe)

data TableUser = TableUser
    { userid    :: Int
    , username  :: Text
    }

data TableFriends = TableFriends
    { strongId :: Int
    , weakId :: Int
    }


createDB :: IO ()
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




addUser ::  Text -> MaybeT IO TableUser
addUser tgusername = do
    conn <- liftIO $ connectSqlite3 "Users.db"
    
    let query = "INSERT OR IGNORE INTO names (tgusername) VALUES (?)"
    _ <- liftIO $ run conn query [toSql tgusername]
    liftIO $ commit conn
    liftIO $ disconnect conn
    liftIO $ putStrLn $ "Attempted to add user " ++ Text.unpack tgusername ++ " to DB"
    getUser tgusername



getUser :: Text -> MaybeT IO TableUser
getUser un = do
    conn <- liftIO $ connectSqlite3 "Users.db"
    let query = "SELECT id, tgusername FROM names WHERE tgusername = ?"
    rows <- liftIO $ quickQuery' conn query [toSql $ Text.unpack un]
    liftIO $ disconnect conn
    case rows of
      [row] -> return $ TableUser { userid = fromSql $ head row
                                       , username = fromSql $ row !! 1
                                       }
      _     -> hoistMaybe Nothing



addFriend :: TableUser -> TableUser -> MaybeT IO TableFriends
addFriend user friend = do
    conn <- liftIO $ connectSqlite3 "Users.db"
    let usrId = userid user
    let frndId = userid friend
    let query = "INSERT OR IGNORE INTO Friends (StrongId, WeakId) VALUES (?,?)"
    res <- liftIO $ run conn query [toSql usrId, toSql frndId]
    liftIO $ commit conn
    liftIO $ putStrLn "Friend added successfully"
    liftIO $ disconnect conn
    case res of
        1 -> return TableFriends { strongId = usrId, weakId = frndId }
        _ -> hoistMaybe Nothing


getFriends :: TableUser -> MaybeT IO [TableUser]
getFriends user = do
    conn <- liftIO $ connectSqlite3 "Users.db"
    let usrId = userid user
    let query = Text.concat $ map Text.pack
            [ "WITH MutualFriends AS ("
            , " SELECT f1.WeakId AS mutual_friend_id"
            , " FROM Friends f1"
            , " JOIN Friends f2 ON f1.WeakId = f2.StrongId"
            , " AND f1.StrongId = f2.WeakId"
            , " WHERE f1.StrongId = ?"
            , ")"
            , " SELECT n.id, n.tgusernames"
            , " FROM names n"
            , " JOIN MutualFriends mf ON n.id = mf.mutual_friend_id;"
            ]
    result <- liftIO $ quickQuery' conn (Text.unpack query) [toSql usrId]
    liftIO $ disconnect conn
    let users = map (\row -> TableUser
                          { userid = fromSql (row !! 0)
                          , username = fromSql (row !! 1)
                          }) result
    return users


