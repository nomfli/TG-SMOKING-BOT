module TG (smokeBot) where
import qualified Data.Text      as Text
import Data.Text                  (Text)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad              (guard)
import Telegram.Bot.API
import Telegram.Bot.Simple
import DB
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

type Model = ()
data Action = AddToSql Text | AddFriend Text Text | Help | SendSmoke Location Text

helpMsgText :: Text.Text
helpMsgText = Text.pack $ "Здарова заядлый курильщик, сейчас данный бот умеет всего ничего: \n"
                      ++ "/help - для просьбы о помощи \n"
                      ++ "/addfriend @yourfriendname для добавления друга в друзья \n"
                      ++ "/smoke для того, чтобы твои друзья увидели, где ты начинаешь покур"


smokeButton :: KeyboardButton
smokeButton = KeyboardButton
    { keyboardButtonText            = Text.pack "SMOKE"
    , keyboardButtonRequestUsers    = Nothing
    , keyboardButtonRequestChat     = Nothing
    , keyboardButtonRequestContact  = Nothing
    , keyboardButtonRequestLocation = Just True
    , keyboardButtonRequestPoll     = Nothing
    , keyboardButtonWebApp          = Nothing
    }

startKeyboard :: ReplyKeyboardMarkup
startKeyboard = ReplyKeyboardMarkup
    { replyKeyboardMarkupKeyboard = [[smokeButton]]
    , replyKeyboardMarkupResizeKeyboard = Just True
    , replyKeyboardMarkupOneTimeKeyboard = Just True
    , replyKeyboardMarkupSelective = Nothing
    , replyKeyboardMarkupInputFieldSelector = Nothing
    , replyKeyboardMarkupIsPersistent = Nothing
    }




smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }




updateToAction :: Update -> Model -> Maybe Action
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
                           Just $ AddToSql username 
            

                        ("/addfriend", friendname) ->  
                            Just $ (AddFriend username) (Text.pack friendname)
            
                        ("/help", _) -> Just Help 
                        
                        _ -> Nothing
                

                (_, Just username, Just loc) -> Just $ ((SendSmoke loc) username)

        Nothing -> Nothing

        





handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    case action of

        AddToSql username -> model <# do
            _ <- liftIO (runMaybeT $ addUser username)
            reply (toReplyMessage helpMsgText)
             { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
            return ()

        AddFriend username friendname -> model <# do
            mu1 <- liftIO (runMaybeT $ getUser username)
            mu2 <- liftIO (runMaybeT $ getUser friendname)
            _ <- case (mu1, mu2) of
                (Just u1, Just u2) -> liftIO $ runMaybeT $ addFriend u1 u2
                _ -> return Nothing
            return ()

        Help -> model <# do
           replyText helpMsgText

            

        SendSmoke loc user -> model <# do

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
