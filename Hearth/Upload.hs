module Upload where

data Upload = Upload
  { id          :: Integer
  , name        :: String
  , roomId      :: Integer
  , userId      :: Integer
  , byteSize    :: Integer
  , contentType :: String
  , fullUrl     :: String
  , createdAt   :: String
  }