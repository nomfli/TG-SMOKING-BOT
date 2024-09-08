{-# LANGUAGE OverloadedStrings #-}
import DB
import TG
import qualified Data.Text                        as Text
import Telegram.Bot.API
import Telegram.Bot.Simple


main :: IO ()
main = do 
  createDB
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  env <- defaultTelegramClientEnv token
  startBot_ smokeBot env

