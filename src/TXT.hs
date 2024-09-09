module TXT (helpMsgText, smokeLocRequest, startKeyboard ) where

import qualified Data.Text                        as Text
import Data.Text (Text)
import Telegram.Bot.API


helpMsgText :: Text.Text
helpMsgText = Text.pack $ "Здарова заядлый курильщик, сейчас данный бот умеет всего ничего: \n"
                      ++ "/help - для просьбы о помощи \n"
                      ++ "/addfriend @yourfriendname для добавления друга в друзья \n"
                      ++ "/smoke для того, чтобы твои друзья увидели, где ты начинаешь покур"





smokeLocRequest :: Float -> Float -> Integer -> SendLocationRequest
smokeLocRequest lan lon chatid = SendLocationRequest
    { sendLocationBusinessConnectionId      = Nothing
    , sendLocationChatId                    = SomeChatId $ ChatId chatid
    , sendLocationMessageThreadId           = Nothing
    , sendLocationLatitude                  = lan
    , sendLocationLongitude                 = lon
    , sendLocationHorizontalAccuracy        = Nothing
    , sendLocationLivePeriod                = 60
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






