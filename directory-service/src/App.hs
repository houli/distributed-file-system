{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks, liftIO, ReaderT)
import Data.ByteString.Char8 (pack)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql ((=.), entityKey, entityVal, get, getBy, insertBy,
                                    selectFirst, selectList, updateGet, ConnectionPool, SelectOpt(..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Auth.Client (Token(..))
import Servant.Client (parseBaseUrl)

import AuthAPI.Client (authAPIClient)
import Config (Config(..))
import DirectoryAPI.API (directoryAPIProxy, DirectoryAPI)
import Models (runDB, EntityField(..), File(..), Node(..), NodeId, Unique(..))

type App = ReaderT Config Handler

appToServer :: Config -> Server DirectoryAPI
appToServer cfg = enter (runReaderTNat cfg) server

app :: ConnectionPool -> IO Application
app pool = do
  manager <- newManager defaultManagerSettings
  authBase <- parseBaseUrl "http://auth-service:8080"
  pure $ serve directoryAPIProxy (appToServer $ Config pool manager authBase)

server :: ServerT DirectoryAPI App
server = ls :<|> whereis :<|> roundRobinNode :<|> registerFileServer

ls :: Maybe String -> App [File]
ls maybeToken = authenticate maybeToken $
  (entityVal <$>) <$> runDB (selectList [] [])

whereis :: Maybe String -> FilePath -> App Node
whereis maybeToken path = authenticate maybeToken $ do
  maybeFile <- runDB $ getBy $ UniquePath $ filter ('/' /=) path
  case maybeFile of
    Nothing -> throwError err404 -- File path does not exist
    Just file -> do
      maybeNode <- runDB . get . fileNode $ entityVal file
      case maybeNode of
        Nothing -> throwError err500 { errBody = "File node is no longer accessible" }
        Just node -> pure node

roundRobinNode :: Maybe String -> App Node
roundRobinNode maybeToken = authenticate maybeToken $ do
  maybeLeastRecentNode <- runDB $ selectFirst [] [Asc NodeStoredAt] -- Sort nodes by date ascending to find LRU
  case maybeLeastRecentNode of
    Nothing -> throwError err500 { errBody = "No nodes registered with directory service" }
    Just leastRecentNode -> do
      currentTime <- liftIO getCurrentTime
      updatedNode <- runDB $ updateGet (entityKey leastRecentNode) [NodeStoredAt =. currentTime]
      pure $ entityVal leastRecentNode

registerFileServer :: Int -> App NodeId
registerFileServer port = do
  currentTime <- liftIO getCurrentTime
  newOrExistingNode <- runDB $ insertBy Node { nodePort = port, nodeStoredAt = currentTime }
  case newOrExistingNode of
    Left _ -> throwError err500 { errBody = "Node with that port already exists" }
    Right newNode -> pure newNode

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
