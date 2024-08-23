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

data Action = AddToSql Text

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
  text <- messageText msg
  if text == "/start" then do
    user <- messageFrom msg
    let username = fromMaybe "Nothing" (userUsername user)
    return $ AddToSql username
  else
    Nothing


handleAction :: Action -> Model -> Eff Action Model
handleAction (AddToSql username) model = model <# do
  liftIO $ do
    conn <- connectSqlite3 "names.db"
    addUser conn username
    disconnect conn
    return model



addUser :: Connection -> Text -> IO ()
addUser conn tgID = do
  let query = "INSERT INTO tgId (tgID) VALUES (?)"
  run conn query [toSql tgID]
  commit conn
  putStrLn "User aded to db"


main :: IO ()
main = do
  conn <- connectSqlite3 "names.db"
  run conn "CREATE TABLE IF NOT EXISTS tgId (ID INTEGER PRIMARY KEY AUTOINCREMENT, tgID TEXT UNIQUE)" []
  commit conn
  disconnect conn
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  env <- defaultTelegramClientEnv token
  startBot_ smokeBot env
