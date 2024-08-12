{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Maybe

import Database.HDBC ()
import Database.HDBC.Sqlite3()

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Database.HDBC
import Database.HDBC.Sqlite3



{-type Model = ()

data Action
  = InlineEcho InlineQueryId Data.Text.Text
  | StickerEcho InputFile ChatId
  | Echo Data.Text.Text

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateInlineQuery update =  do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
      let msg =  inlineQueryQuery query
      Just $ InlineEcho queryId msg
  | isJust $ updateMessageSticker update = do
    fileId <- stickerFileId <$> updateMessageSticker update
    chatOfUser <- updateChatId update
    pure $ StickerEcho (InputFileId fileId) chatOfUser
  | otherwise = case updateMessageText update of
      Just text -> Just (Echo text)
      Nothing   -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  InlineEcho queryId msg -> model <# do
    let result = (defInlineQueryResultGeneric (InlineQueryResultId msg))
          { inlineQueryResultTitle = Just msg
          , inlineQueryResultInputMessageContent = Just (defaultInputTextMessageContent msg)
          }
        thumbnail = defInlineQueryResultGenericThumbnail result
        article = defInlineQueryResultArticle thumbnail
        answerInlineQueryRequest = defAnswerInlineQuery queryId [article]
    _ <- runTG  answerInlineQueryRequest
    return ()
  StickerEcho file chat -> model <# do
    _ <- runTG
        (defSendSticker (SomeChatId chat) file)
    return ()
  Echo msg -> model <# do
    pure msg -- or replyText msg

runs :: Token -> IO ()
runs token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  runs token
-}



data Action = AddToSql Text

smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ 
  | Just text <- updateMessageText update, text == "/start" = do
      user <- messageFrom <$> updateMessage update
      return $ AddToSql $ userFirstName user
  | otherwise = Nothing 


handleAction :: Action -> Model -> Eff Action Model
handleAction (AddToSql user) _ = liftIO $ addUser conn user  






addUser :: Connection -> Text -> IO ()
addUser conn tgID = do
  let query = "INSERT INTO names (tgID) VALUES (?)"
  _ <- run conn query [toSql tgID]
  commit conn



main :: IO ()
main = do
  conn <- connectSqlite3 "names.db"
  let token = Token "7490390509:AAGbMO5WlttwqPbGJ4UzZirpX1jacN2JJ24"
  env <- defaultTelegramClientEnv token
  startBot_ smokeBot env