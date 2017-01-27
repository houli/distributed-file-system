module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Client (parseBaseUrl)
import System.Directory (createDirectoryIfMissing)

import App (app)
import DirectoryAPI.Client (directoryAPIClient)
import Models (connStr, NodeId)

main :: IO ()
main = do
  createDirectoryIfMissing True "/data/files" -- Create any missing directory structure for our file storage
  nodeId <- registerWithDirectoryService
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  application <- app pool nodeId
  run 8080 $ logStdoutDev application

-- Registers a file server node with the directory service
-- TODO: Get exposed port from environment
registerWithDirectoryService :: IO NodeId
registerWithDirectoryService = do
  manager <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl "http://directory-service:8080"
  response <- runExceptT $ registerFileServer 8081 manager baseUrl
  case response of
    Left _ -> error "Unable to register with directory service"
    Right nodeId -> pure nodeId

-- Pattern match out the file server registration endpoint
_ :<|> _ :<|> registerFileServer = directoryAPIClient
