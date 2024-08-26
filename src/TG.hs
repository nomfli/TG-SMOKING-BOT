module TG where

import Telegram.Bot.API
import Telegram.Bot.Simple
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Data.Maybe (fromMaybe)


import Control.Monad.IO.Class (liftIO)
import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import DB

type Model = ()
data Action = AddToSql Text | AddFriend Text Text


smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing



updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = 
  updateMessage update >>= 
  \msg -> messageText msg >>= 
  \mText -> 
    let wordsText = Text.words mText
        command = head wordsText
    in guard (not $ null wordsText) *> 
       case command of
         cmd | cmd == Text.pack "/start" -> 
           let username = fromMaybe "Nothing" (fmap Text.unpack (userUsername =<< messageFrom msg))
           in Just (AddToSql (Text.pack username))
         cmd | cmd == Text.pack "/addfriend" ->
           guard (length wordsText == 2) *> 
           let friendname = last wordsText
               username = fromMaybe "Nothing" (fmap Text.unpack (userUsername =<< messageFrom msg))
           in guard (Text.head friendname == '@') *>
              Just (AddFriend (Text.pack username) friendname)
         _ -> Nothing



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








