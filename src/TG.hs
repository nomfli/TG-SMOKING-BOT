<<<<<<< HEAD
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
=======
module TG (smokeBot) where
import qualified Data.Text      as Text
import Data.Text                  (Text)
import Control.Monad.IO.Class     (liftIO)
import Telegram.Bot.API
import Data.Maybe                 (fromMaybe)
import Telegram.Bot.Simple
import DB                       
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT), hoistMaybe)
import TXT 


>>>>>>> mr


smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


<<<<<<< HEAD
guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing
=======
type Model = ()
data Action = AddToSql Text Integer | AddFriend Text Text | Help | SendSmoke Location (Maybe User) | DeleteFriend Text Text





>>>>>>> mr



updateToAction :: Update -> Model -> Maybe Action
<<<<<<< HEAD
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
=======
updateToAction update _ =
  case updateMessage update of
        Just msg -> 
            case (messageText msg, userUsername =<< messageFrom msg, messageLocation msg) of

                (Just mText, Just username, _) ->
                 let wordsText = Text.words mText
                     command   = Prelude.head wordsText
                     arg      = case length wordsText of
                            2 -> Text.unpack $ last wordsText
                            _ ->  ""
                    in 
    
                    case (Text.unpack command, arg) of

                        ("/start", "") ->
                            (AddToSql username) <$> (fmap (read . (flip (!!) 1) . words . show .userId) $ messageFrom msg)
            

                        ("/addfriend", friendname) ->  
                            Just $ (AddFriend username) (Text.pack friendname)
            
                        ("/help", _) -> Just Help 
                        
                        ("/deletefriend", friendname) -> Just $ DeleteFriend (username) (Text.pack friendname) 

                        _ -> Nothing
                

                (_, Just username, Just loc) -> Just $ SendSmoke loc (messageFrom msg)
                _ -> Nothing

        Nothing -> Nothing

        


>>>>>>> mr



handleAction :: Action -> Model -> Eff Action Model
<<<<<<< HEAD
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
=======
handleAction action model =
    case action of

        AddToSql username chatId -> model <# do
            _ <- liftIO (runMaybeT $ addUser username chatId)
            reply (toReplyMessage helpMsgText)
             { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
            return ()

        AddFriend username friendname -> model <# do
            mu1 <- liftIO (runMaybeT $ getUser username)
            mu2 <- liftIO (runMaybeT $ getUser friendname)
            friendsConnection <- case (mu1, mu2) of
                (Just u1, Just u2) -> liftIO $ runMaybeT $ addFriend u1 u2
                _ -> return Nothing
            case friendsConnection of
              Just _ -> liftIO $ putStrLn "Friends added"
              _ -> liftIO $ putStrLn "GG"
            return ()

        Help -> model <# do
           replyText helpMsgText

            

        SendSmoke loc user -> model <# do
            
>>>>>>> mr





<<<<<<< HEAD
=======
            let username = ((=<<) :: (User -> Maybe Text) -> Maybe User -> Maybe Text) userUsername user
            xs <- case username of
                Just us ->
                       liftIO (runMaybeT $ getFriends =<< (getUser us))
                _ -> return Nothing
                        
            let lon = locationLongitude loc
            let lan = locationLatitude  loc
            let xxs = fromMaybe [] xs
            _ <- mapM_ runTG $ map (smokeLocRequest lan lon . DB.chatId) xxs 
            return ()
            
        _ -> return model
>>>>>>> mr



