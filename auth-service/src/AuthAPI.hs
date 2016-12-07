{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module AuthAPI where

import Data.Aeson
import GHC.Generics
import GHC.Int
import Servant

import Models (User)

type AuthAPI = "createUser" :> ReqBody '[JSON] User :> PostCreated '[JSON] UserCreationResponse

newtype UserCreationResponse = UserCreationResponse
  { userId :: Int64 } deriving (Generic, ToJSON)
