{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module App
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.PasswordStore (makePassword)
import Data.ByteString.Char8 (pack, unpack)
import Database.Persist.Postgresql (fromSqlKey, insertBy, ConnectionPool)
import Servant

import AuthAPI (AuthAPI, UserCreationResponse(..))
import Models (runDb, User(..))

newtype App a = App
              { runApp :: ReaderT ConnectionPool Handler a
              } deriving (Functor, Applicative, Monad, MonadReader ConnectionPool,
                          MonadError ServantErr, MonadIO)

appToServer :: ConnectionPool -> Server AuthAPI
appToServer pool = enter (convertApp pool) server

convertApp :: ConnectionPool -> App :~> Handler
convertApp pool = Nat (flip runReaderT pool . runApp)

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
