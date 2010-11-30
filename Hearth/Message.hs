module Message where 
import Text.JSON
import Prelude hiding (id)
--import Data.Map

lookupM a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

data Message = Message
  { id          :: Integer 
  , roomId      :: Integer
  , userId      :: Integer
  , body        :: String
  , createdAt   :: String
  , messageType :: MessageType    
  }

data MessageType =
   TextMessage
 | PasteMessage
 | SoundMessage
 | AdvertisementMessage
 | AllowGuestMessage
 | DisallowGuestMessage
 | IdleMessage
 | KickMessage
 | LeaveMessage
 | SystemMessage
 | TimestampMessage
 | TopicChangeMessage
 | UnidleMessage
 | UnlockMessage
 | UploadMessage
 deriving (Show)
 
instance JSON Message where
  readJSON (JSObject obj) = do
    let objA = fromJSObject obj
    id <- lookupM "id" objA >>= readJSON
    roomId <- lookupM "roomId" objA >>= readJSON
    userId <- lookupM "userId" objA >>= readJSON
    body <- lookupM "body" objA >>= readJSON
    createdAt <- lookupM "createdAt" objA >>= readJSON
    messageType <- lookupM "type" objA >>= readJSON
    return $ Message id roomId userId body createdAt messageType
  showJSON mess = makeObj
    [ ("id", showJSON $ id mess)
    , ("roomId", showJSON $ roomId mess)
    , ("userId", showJSON $ userId mess)
    , ("body", showJSON $ body mess)
    , ("createdAt", showJSON $ createdAt mess)
    , ("type", showJSON $ messageType mess)
    ]

instance JSON MessageType where
  readJSON (JSString jstr) = do
    return $
      case (fromJSString jstr) of
      "TextMessage" -> TextMessage
      "PasteMessage" -> PasteMessage
      "SoundMessage" -> SoundMessage
      "AdvertisementMessage" -> AdvertisementMessage
      "AllowGuestMessage" -> AllowGuestMessage
      "DisallowGuestMessage" -> DisallowGuestMessage
      "IdleMessage" -> IdleMessage
      "KickMessage" -> KickMessage
      "LeaveMessage" -> LeaveMessage
      "SystemMessage" -> SystemMessage
      "TimestampMessage" -> TimestampMessage
      "TopicChangeMessage" -> TopicChangeMessage
      "UnidleMessage" -> UnidleMessage
      "UnlockMessage" -> UnlockMessage
      "UploadMessage" -> UploadMessage
  showJSON mt = JSString $ toJSString $ show mt