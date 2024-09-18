module TXT (helpMsgText, smokeLocRequest, startKeyboard, smokeMesRequest, deleteFriendMsg, addFriendMsg, deleteFriendBadMsg, addFriendBadMsg) where

import qualified Data.Text                        as Text
import Data.Text (Text)
import Telegram.Bot.API


helpMsgText :: Text.Text
helpMsgText = Text.pack $ "Здарова заядлый курильщик, сейчас данный бот умеет всего ничего: \n"
                      ++ "/help - для просьбы о помощи \n"
                      ++ "/addfriend yourfriendname для добавления друга в друзья(без собачки) \n"
                      ++ "/deletefriend yourfriendname для удаления дружбы \n"
                      ++ "/friendlist можешь посмотреть на самых знатных курильщиков \n"
                      ++ "и большая кнопка SMOKE, для того, чтобы вызвать всех своих друзей на покур \n"
                      ++ "важно, чтобы у тебя было полное разрешение GPS к твоему ТГ"



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


deleteFriendMsg :: Text -> Text -> Text 
deleteFriendMsg username friendname = username `Text.append` (Text.pack "deleted ") `Text.append` friendname `Text.append` (Text.pack " from the friendlist") 


addFriendMsg :: Text -> Text -> Text
addFriendMsg username friendname = username `Text.append` (Text.pack " added ") `Text.append` friendname `Text.append` (Text.pack " to the friendlist") 

addFriendBadMsg :: Text -> Text -> Text 
addFriendBadMsg username friendname = (Text.pack "Error ") `Text.append` username
                                         `Text.append` (Text.pack " can't add ")
                                         `Text.append` friendname  `Text.append` (Text.pack " to friends ") 


deleteFriendBadMsg :: Text -> Text -> Text 
deleteFriendBadMsg  username friendname = (Text.pack "Error ") `Text.append` username
                                         `Text.append` (Text.pack " can't delete ")
                                         `Text.append` friendname  `Text.append` (Text.pack " from friends ") 


