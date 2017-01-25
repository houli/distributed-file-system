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

import AuthAPI.Client (authAPIClient)
import Config (Config(..))
import DirectoryAPI.API(directoryAPIProxy, DirectoryAPI)
import Models (runDB, File(..), Node, Unique(..))

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
ls maybeToken = authenticate maybeToken $ do
  allFiles <- runDB $ selectList [] []
  pure $ entityVal <$> allFiles

whereis :: Maybe String -> FilePath -> App Node
whereis maybeToken path = authenticate maybeToken $ do
  maybeFile <- runDB $ getBy $ UniquePath path
  case maybeFile of
    Nothing -> throwError err404 -- File path does not exist
    Just file -> do
      maybeNode <- runDB . get . fileNode $ entityVal file
      case maybeNode of
        Nothing -> throwError err500 { errBody = "File node is no longer accessible" }
        Just node -> pure node

registerFileServer :: App NoContent
registerFileServer = undefined

-- General authentication function, performs action only when succesfully authenticated
authenticate :: Maybe String -> App a -> App a
authenticate Nothing _ = throwError err401 { errBody = "No authentication token provided" }
authenticate (Just token) action = do
  manager <- asks manager
  authBase <- asks authBase
  response <- liftIO $ runExceptT $ verifyJWT (Token $ pack token) manager authBase
  case response of
    Left _ -> throwError err401 { errBody = "Invalid authentication token" }
    Right _ -> action

-- Pattern match out the routes of the AuthAPI
_ :<|> _ :<|> verifyJWT = authAPIClient
