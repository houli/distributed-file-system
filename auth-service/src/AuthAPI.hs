{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module AuthAPI where

import Database.Persist.Postgresql (Entity)
import Servant

import Models (User)

type AuthAPI = "users" :> Get '[JSON] [Entity User]
