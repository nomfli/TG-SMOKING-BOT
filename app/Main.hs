{-# LANGUAGE OverloadedStrings #-}
import DB
import TG
import Control.Monad.IO.Class (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Data.Maybe (fromMaybe)


import Control.Monad.IO.Class (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)

main :: IO ()
main = do 
  createDB
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  env <- defaultTelegramClientEnv token
  startBot_ smokeBot env
