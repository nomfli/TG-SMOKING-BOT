module TXT (helpMsgText, smokeLocRequest, startKeyboard, smokeMesRequest) where

import qualified Data.Text                        as Text
import Data.Text (Text)
import Telegram.Bot.API


helpMsgText :: Text.Text
helpMsgText = Text.pack $ "Здарова заядлый курильщик, сейчас данный бот умеет всего ничего: \n"
                      ++ "/help - для просьбы о помощи \n"
                      ++ "/addfriend @yourfriendname для добавления друга в друзья \n"
                      ++ "/smoke для того, чтобы твои друзья увидели, где ты начинаешь покур"



smokeMesRequest :: Text -> Integer -> SendMessageRequest 
smokeMesRequest username chatid = SendMessageRequest
    { sendMessageBusinessConnectionId   = Nothing 
    , sendMessageChatId                 = SomeChatId $ ChatId chatid
    , sendMessageMessageThreadId        = Nothing 
    , sendMessageText                   = Text.pack $ "Твой друг " ++ Text.unpack username ++ " вышел покурить"
    , sendMessageParseMode              = Nothing 
    , sendMessageEntities               = Nothing 
    , sendMessageLinkPreviewOptions     = Nothing 
    , sendMessageDisableNotification    = Nothing 
    , sendMessageProtectContent         = Nothing 
    , sendMessageMessageEffectId        = Nothing 
    , sendMessageReplyToMessageId       = Nothing 
    , sendMessageReplyParameters        = Nothing 
    , sendMessageReplyMarkup            = Nothing 
    }



smokeLocRequest :: Float -> Float -> Integer -> SendLocationRequest
smokeLocRequest lan lon chatid = SendLocationRequest
    { sendLocationBusinessConnectionId      = Nothing
    , sendLocationChatId                    = SomeChatId $ ChatId chatid
    , sendLocationMessageThreadId           = Nothing
    , sendLocationLatitude                  = lan
    , sendLocationLongitude                 = lon
    , sendLocationHorizontalAccuracy        = Nothing
    , sendLocationLivePeriod                = 300
    , sendLocationHeading                   = Nothing
    , sendLocationProximityAlertRadius      = Nothing
    , sendLocationDisableNotification       = Nothing
    , sendLocationProtectContent            = Nothing
    , sendLocationMessageEffectId           = Nothing
    , sendLocationReplyToMessageId          = Nothing
    , sendLocationReplyParameters           = Nothing
    , sendLocationReplyMarkup               = Nothing
    }




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






