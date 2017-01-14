{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8 (pack, unpack)
import           Database.Persist.Postgresql
import           GHC.Int (Int64)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant
import           Servant.Auth.Client
import           Servant.Client
import           System.Directory (doesFileExist)

import           AuthAPI (authAPIClient)
import           Config
import           FileAPI (fileAPIProxy, FileAPI, HTTPFile(..))
import           Models

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
    Just token -> authenticate (Token $ pack token) $ do
      let filePath = "/data/files/" ++ path
      exists <- liftIO $ doesFileExist filePath
      if exists
      then do
        fileContents <- liftIO $ BS.readFile filePath
        pure HTTPFile { path = path, contents = unpack $ encode fileContents }
      else throwError err404


writeFileImpl :: Maybe String -> HTTPFile -> App NoContent
writeFileImpl = undefined

authenticate :: Token -> App a -> App a
authenticate token action = do
  manager <- asks manager
  authBase <- asks authBase
  res <- liftIO $ runExceptT $ verifyJWT token manager authBase
  case res of
    Left _ -> throwError err401 { errBody = "Invalid authentication token" }
    Right _ -> action

_ :<|> _ :<|> verifyJWT = authAPIClient
