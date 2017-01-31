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

-- Type alias custom monad to handle passing our configuration around
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

-- Return all file records from the database
ls :: Maybe String -> App [File]
ls maybeToken = authenticate maybeToken $
  (entityVal <$>) <$> runDB (selectList [] [])

findFile :: FilePath -> App (Maybe File)
findFile path = do
  maybeFile <- runDB $ getBy $ UniquePath path
  case maybeFile of
    Nothing -> pure Nothing
    Just file -> pure $ Just $ entityVal file

-- Return the primary node that the file is stored on
whereis :: Maybe String -> FilePath -> App Node
whereis maybeToken path = authenticate maybeToken $ do
  maybeFile <- findFile path
  case maybeFile of
    Nothing -> throwError err404 -- File path does not exist
    Just file -> do
      maybeNode <- runDB . get . fileNode $ file
      case maybeNode of
        Nothing -> throwError err500 { errBody = "File node is no longer accessible" }
        Just node -> pure node

-- Primary node that the file is stored on or the next round robin primary
roundRobinNode :: Maybe String -> FilePath -> App Node
roundRobinNode maybeToken path = authenticate maybeToken $ do
  maybeFile <- findFile path
  case maybeFile of
    Nothing -> do
      maybeLeastRecentNode <- runDB $ selectFirst [] [Asc NodeStoredAt] -- Sort nodes by date ascending to find LRU
      case maybeLeastRecentNode of
        Nothing -> throwError err500 { errBody = "No nodes registered with directory service" }
        Just leastRecentNode -> do
          currentTime <- liftIO getCurrentTime
          runDB $ updateGet (entityKey leastRecentNode) [NodeStoredAt =. currentTime]
    Just file -> do
      maybeNode <- runDB $ get (fileNode file)
      case maybeNode of
        Nothing -> throwError err500 { errBody = "File found but no node found" }
        Just node -> pure node

registerFileServer :: Int -> App NodeId
registerFileServer port = do
  currentTime <- liftIO getCurrentTime -- Node last stored at time, default current time
  newOrExistingNode <- runDB $ insertBy Node { nodePort = port, nodeStoredAt = currentTime }
  case newOrExistingNode of
    Left existingNode -> pure $ entityKey existingNode
    Right newNode -> pure newNode

-- General authentication function, performs action only when succesfully authenticated
authenticate :: Maybe String -> App a -> App a
authenticate Nothing _ = throwError err401 { errBody = "No authentication token provided" }
authenticate (Just token) action = do
  manager <- asks manager
  authBase <- asks authBase
  response <- liftIO $ runExceptT $ verifyJWT (Token $ pack token) manager authBase -- Verify token
  case response of
    Left _ -> throwError err401 { errBody = "Invalid authentication token" }
    Right _ -> action

-- Pattern match out the routes of the AuthAPI
_ :<|> _ :<|> verifyJWT = authAPIClient
