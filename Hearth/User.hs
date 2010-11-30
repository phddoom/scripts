module User where

data User = User
  { id           :: Integer
  , name         :: String
  , emailAddress :: String
  , admin        :: Bool
  , createdAt    :: String
  , userType     :: UserType
  }
  
data UserType = Member | Guest