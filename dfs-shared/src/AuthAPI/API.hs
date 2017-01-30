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

type AuthAPI auths = "register" :> -- Endpoint to register a new user
                      ReqBody '[JSON] User :> -- Details of user to be registered
                      Post '[JSON] (Headers '[Header "Token" ByteString] UserCreationResponse) -- Return auth token

                :<|> "login" :> -- Endpoint to login to an existing user account
                      ReqBody '[JSON] User :> -- Details of user to be logged in
                      PostNoContent '[JSON] (Headers '[Header "Token" ByteString] NoContent) -- Return auth token

                :<|> Auth auths User :> -- Verify token before responding
                     "verifyJWT" :> -- Endpoint to verify a JWT was signed by this server
                     Get '[JSON] NoContent -- Respond success if verified

-- Common auth token header combinator
type AuthToken = Header "Authorization" String

newtype UserCreationResponse = UserCreationResponse
  { userId :: Int64 } deriving (Generic, FromJSON, ToJSON)

authAPIProxy :: Proxy (AuthAPI '[JWT])
authAPIProxy = Proxy
