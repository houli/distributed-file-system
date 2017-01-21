{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks, liftIO, ReaderT)
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Auth.Client (Token(..))
import Servant.Client

import AuthAPI (authAPIClient)
import Config (Config(..))
import DirectoryAPI (directoryAPIProxy, DirectoryAPI)
import Models (runDB, File, Node)

type App = ReaderT Config Handler

appToServer :: Config -> Server DirectoryAPI
appToServer cfg = enter (runReaderTNat cfg) server

app :: ConnectionPool -> IO Application
app pool = do
  manager <- newManager defaultManagerSettings
  authBase <- parseBaseUrl "http://auth-service:8080"
  pure $ serve directoryAPIProxy (appToServer $ Config pool manager authBase)

server :: ServerT DirectoryAPI App
server = ls :<|> whereis :<|> registerFileServer

ls :: Maybe String -> App [File]
ls maybeToken =
  case maybeToken of
    Nothing -> throwError err401 { errBody = "No authentication token provided" }
    Just token -> authenticate (Token $ pack token) $ do
      allFiles <- runDB $ selectList [] []
      pure $ entityVal <$> allFiles

whereis :: Maybe String -> App Node
whereis = undefined

registerFileServer :: App NoContent
registerFileServer = undefined

-- General authentication function, performs action only when succesfully authenticated
authenticate :: Token -> App a -> App a
authenticate token action = do
  manager <- asks manager
  authBase <- asks authBase
  res <- liftIO $ runExceptT $ verifyJWT token manager authBase
  case res of
    Left _ -> throwError err401 { errBody = "Invalid authentication token" }
    Right _ -> action

-- Pattern match out the routes of the AuthAPI
_ :<|> _ :<|> verifyJWT = authAPIClient
