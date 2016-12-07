{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module App
  ( app
  ) where

import Control.Monad.Reader
import Crypto.PasswordStore (makePassword)
import Data.ByteString.Char8 (pack, unpack)
import Database.Persist.Postgresql (fromSqlKey, insertBy, ConnectionPool)
import Servant

import AuthAPI (AuthAPI, UserCreationResponse(..))
import Models (runDb, User(..))

type App = ReaderT ConnectionPool Handler

appToServer :: ConnectionPool -> Server AuthAPI
appToServer pool = enter (runReaderTNat pool) server

api :: Proxy AuthAPI
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api (appToServer pool)

server :: ServerT AuthAPI App
server = createUser

createUser :: User -> App UserCreationResponse
createUser user = do
  hashedPassword <- liftIO $ makePassword (pack $ userPassword user) 17
  newOrExistingUser <- runDb $ insertBy user { userPassword = unpack hashedPassword }
  case newOrExistingUser of
    Left _ -> throwError err500 { errBody = "Username already taken" }
    Right newUser -> pure UserCreationResponse { userId = fromSqlKey newUser}
