{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Lib
    ( app
    ) where

import Data.Aeson
import GHC.Generics
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, FromJSON, Generic, Show, ToJSON)

type API = "users" :> Get '[JSON] [User]

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = pure users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
