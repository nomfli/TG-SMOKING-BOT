module TG (smokeBot) where  
    
import qualified Data.Text      as Text
import Data.Text                  (Text, snoc)
import Data.Text.IO               (putStrLn)
import Control.Monad.IO.Class     (liftIO)
import Telegram.Bot.API
import Data.Maybe                 (fromMaybe)
import Telegram.Bot.Simple
import DB                       
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT), hoistMaybe)
import TXT 


smokeBot :: BotApp Model Action
smokeBot = BotApp
  { botInitialModel = ()  
  , botAction       = updateToAction
  , botHandler      = handleAction
  , botJobs         = []
  }


type Model = ()  
data Action = AddToSql Text Integer | AddFriend Text Text | Help | SendSmoke Location (Maybe User) | DeleteFriend Text Text | FriendLists Text  

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
                           AddToSql username <$> (read . (!! 1) . words . show .userId) <$> messageFrom msg

                        ("/addfriend", friendname) ->  
                            Just $ AddFriend username (Text.pack $ friendname)
                        
                        ("/deletefriend", friendname) -> 
                            Just $ DeleteFriend username (Text.pack $ friendname) 
            
                        ("/help","") -> Just Help 
                        
                        ("/friendlist", "") -> Just $ FriendLists username 

                        _ -> Nothing
                

                (_, Just username, Just loc) -> Just $ SendSmoke loc (messageFrom msg)
                _ -> Nothing

        Nothing -> Nothing

        

handleAction :: Action -> Model -> Eff Action Model
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
                Just _ -> do 
                    liftIO $ Data.Text.IO.putStrLn $ addFriendMsg username friendname
                    replyText $ addFriendMsg username friendname 
                _ -> do 
                    liftIO $ Data.Text.IO.putStrLn $ Text.append username (Text.append (Text.pack " can't add ") friendname)
                    replyText $ addFriendBadMsg username friendname
            return ()

        Help -> model <# do
           replyText helpMsgText

        
        DeleteFriend username friendname -> model <# do
            mu1 <- liftIO (runMaybeT $ getUser username ) 
            mu2 <- liftIO (runMaybeT $ getUser friendname )
            xs <- case (mu1 , mu2 ) of
                    (Just usr, Just friend) -> liftIO $ runMaybeT $ deleteFriend usr friend
                    _ -> return Nothing
            case xs of
                Just _ -> do  
                    liftIO $ Data.Text.IO.putStrLn $ deleteFriendMsg username friendname   
                    replyText $ deleteFriendMsg username friendname   

                _ -> do
                    liftIO $ Data.Text.IO.putStrLn $ Text.append  username (Text.append (Text.pack " can't deleted ") friendname)
                    replyText $ deleteFriendBadMsg username friendname 
            return() 



        SendSmoke loc user -> model <# do
            let username = ((=<<) :: (User -> Maybe Text) -> Maybe User -> Maybe Text) userUsername user
            xs <- case username of
                Just us ->
                       liftIO (runMaybeT $ getFriends =<< getUser us)
                _ -> return Nothing
                 
            let xxs = fromMaybe [] xs
            let realUsrName = fromMaybe (Text.pack "")  username
            _ <- mapM_ runTG $ map (smokeLocRequest (locationLatitude loc) (locationLongitude loc) . DB.chatId) xxs
            _ <- mapM_ runTG $ map (smokeMesRequest realUsrName . DB.chatId) xxs
            return ()
        

        FriendLists username -> model <# do 
                xs  <- liftIO $ runMaybeT (getFriends =<< getUser username)
                case xs of
                  Just flist ->  
                    replyText $ Text.concat $ addNumberToList 1 $ map (\x -> x `Text.append` ( Text.pack " \n")) (map DB.username flist) 
                  Nothing -> return () 
        _ -> return model



addNumberToList :: Integer -> [Text] -> [Text]
addNumberToList _ [] = []
addNumberToList a (x:xs) = ((Text.pack $ show a) `Text.append` (Text.pack ". ") `Text.append` x ) : (addNumberToList (a +  1)) xs
                         
    
