module Room where
import User

data Room = Room
  { id               :: Integer
  , name             :: String
  , topic            :: String
  , membershipLimit  :: Integer
  , full             :: Bool
  , openToGuests     :: Bool
  , activeTokenValue :: String
  , updatedAt        :: String
  , createdAt        :: String
  , users            :: [User]
  }