{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Telegram.Bot.API
import Telegram.Bot.Simple
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Data.Maybe (fromMaybe)

type Model = ()

data Action = AddToSql Text | AddFriend Text Text

smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = do
  msg <- updateMessage update
  mText <- messageText msg
  let wordsText = Text.words mText 
  if not (null wordsText) then
    case head wordsText of
      "/start" -> do
        user <- messageFrom msg
        let username = fromMaybe "Nothing" (userUsername user)
        return $ AddToSql username
      "/addfriend" -> 
        if length wordsText == 2 then do
          user <- messageFrom msg
          let username = fromMaybe "Nothing" (userUsername user)
          let friendname = last wordsText
          if Text.head  friendname == '@' then 
            return $ AddFriend username friendname
          else 
            Nothing
        else
          Nothing
      _ -> Nothing
  else
    Nothing


handleAction :: Action -> Model -> Eff Action Model
handleAction (AddToSql username) model = model <# do
  liftIO $ do
    conn <- connectSqlite3 "names.db"
    addUser conn username
    disconnect conn
    return model
handleAction (AddFriend username fusername) model = model <# do
  liftIO $ do
     fconn <- connectSqlite3 "friends.db"
     addFriend fconn username fusername
     disconnect fconn
     return model

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





main :: IO ()
main = do
  conn <- connectSqlite3 "names.db"
  run conn "CREATE TABLE IF NOT EXISTS tgId (ID INTEGER PRIMARY KEY AUTOINCREMENT, tgID TEXT UNIQUE)" []
  commit conn
  disconnect conn

  fconn <- connectSqlite3 "friends.db"
  run fconn "CREATE TABLE IF NOT EXISTS Friends (ID INTEGER PRIMARY KEY AUTOINCREMENT, WeakID Text, StrongID Text)" []
  run fconn "CREATE UNIQUE INDEX IF NOT EXISTS idx_unique_friendship ON Friends (WeakID, StrongID)" []
  putStrLn "Please, enter Telegram bot's API token:"
  commit fconn
  disconnect fconn

  token <- Token . Text.pack <$> getLine
  env <- defaultTelegramClientEnv token
  startBot_ smokeBot env

