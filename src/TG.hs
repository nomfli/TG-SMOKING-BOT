module TG where
import qualified Data.Text      as Text
import Data.Text                  (Text)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad              (guard)
import Telegram.Bot.API
import Telegram.Bot.Simple
import DB

type Model = ()
data Action = AddToSql Text | AddFriend Text Text | Smoke TableUser




smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


sendMessageRequest :: SomeChatId -> Text -> SendMessageRequest
sendMessageRequest chatId text = SendMessageRequest
    { sendMessageChatId = chatId
    , sendMessageText = text
    , sendMessageParseMode = Nothing
    , sendMessageDisableNotification = Nothing
    , sendMessageReplyToMessageId = Nothing
    , sendMessageReplyMarkup = Nothing

    }


both:: (a -> b) -> (a, a) -> (b, b)
both f (x, x') = (f x, f x')


updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = 
  updateMessage update >>= 
  \msg -> messageText msg >>= 
  \mText -> 
    let wordsText = (Text.words mText)
        command = (Prelude.head wordsText)
    in guard (not $ Prelude.null wordsText) *> 
    case (Text.unpack command) of
        
        "/start" -> 
            AddToSql <$> (Text.append $ Text.singleton  '@') <$> (userUsername =<< (messageFrom msg))

        "/addfriend" ->
         guard (Prelude.length wordsText == 2)
            *> guard (Text.head (Prelude.last wordsText) == '@')
            *> (AddFriend <$> (userUsername =<< (messageFrom msg)) <*> (Just $ Prelude.last wordsText))
        
        _ -> Nothing
    


handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
    case action of

        AddToSql username -> model <# liftIO
            (addUser username)

        AddFriend username friendname -> model <# liftIO
          (addFriend =<< (sequenceA . both getUser) (username, Text.tail friendname))




