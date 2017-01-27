{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module AuthAPI.API
  ( authAPIProxy
  , AuthAPI
  , AuthToken
  , UserCreationResponse(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant ((:<|>), (:>), Get, Header, Headers, JSON, NoContent, Post, PostNoContent, Proxy(..), ReqBody)
import Servant.Auth.Client ()
import Servant.Auth.Server (Auth, JWT)

import Models (User)

type AuthAPI auths = "register" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "Token" ByteString] UserCreationResponse)
                :<|> "login" :> ReqBody '[JSON] User :> PostNoContent '[JSON] (Headers '[Header "Token" ByteString] NoContent)
                :<|> Auth auths User :> "verifyJWT" :> Get '[JSON] NoContent

-- Common auth token header combinator
type AuthToken = Header "Authorization" String

newtype UserCreationResponse = UserCreationResponse
  { userId :: Int64 } deriving (Generic, FromJSON, ToJSON)

authAPIProxy :: Proxy (AuthAPI '[JWT])
authAPIProxy = Proxy
