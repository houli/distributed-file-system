{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module AuthAPI
  ( AuthAPI
  , UserCreationResponse(..)
  ) where

import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics
import GHC.Int
import Servant
import Servant.Auth.Server

import Models (User)

type AuthAPI auths = "register" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "Token" ByteString] UserCreationResponse)
                :<|> "login" :> ReqBody '[JSON] User :> PostNoContent '[JSON] (Headers '[Header "Token" ByteString] NoContent)
                :<|> Auth auths User :> "verifyJWT" :> Get '[JSON] NoContent

newtype UserCreationResponse = UserCreationResponse
  { userId :: Int64 } deriving (Generic, ToJSON)
