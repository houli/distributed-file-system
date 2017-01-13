{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
import GHC.Int (Int64)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Servant.Auth.Client

import AuthAPI (authAPIClient)
import Config
import FileAPI
import Models

type App = ReaderT Config Handler

appToServer :: Config -> Server FileAPI
appToServer cfg = enter (runReaderTNat cfg) server

app :: ConnectionPool -> Int64 -> IO Application
app pool serverId = do
  manager <- newManager defaultManagerSettings
  authBase <- parseBaseUrl "http://auth-service:8080"
  pure $ serve fileAPIProxy (appToServer $ Config pool serverId manager authBase)

server :: ServerT FileAPI App
server = readFileImpl :<|> writeFileImpl

readFileImpl :: Maybe String -> String -> App HTTPFile
readFileImpl maybeToken path =
  case maybeToken of
    Nothing -> throwError err401 { errBody = "No authentication token provided" }
    Just token -> do
      manager <- asks manager
      authBase <- asks authBase
      res <- liftIO $ runExceptT $ verifyJWT (Token $ pack token) manager authBase
      case res of
        Left _ -> throwError err401 { errBody = "Invalid authentication token" }
        Right _ -> pure NoContent

writeFileImpl = undefined

_ :<|> _ :<|> verifyJWT = authAPIClient
