{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module App
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Database.Persist.Postgresql (selectList, ConnectionPool, Entity)
import Servant

import AuthAPI (AuthAPI)
import Models (User, runDb)

newtype App a = App
              { runApp :: ReaderT ConnectionPool (ExceptT ServantErr IO) a
              } deriving (Functor, Applicative, Monad, MonadReader ConnectionPool,
                          MonadError ServantErr, MonadIO)

appToServer :: ConnectionPool -> Server AuthAPI
appToServer pool = enter (convertApp pool) server

convertApp :: ConnectionPool -> App :~> ExceptT ServantErr IO
convertApp pool = Nat (flip runReaderT pool . runApp)

api :: Proxy AuthAPI
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api (appToServer pool)

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

server :: ServerT AuthAPI App
server = allUsers
