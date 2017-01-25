{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader (asks, liftIO, ReaderT)
import qualified Data.ByteString as BS
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.ByteString.Char8 (pack, unpack)
import           Database.Persist.Postgresql
import           GHC.Int (Int64)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant
import           Servant.Auth.Client (Token(..))
import           Servant.Client (parseBaseUrl)
import           System.Directory (doesFileExist)

import           AuthAPI.Client (authAPIClient)
import           Config (Config(..))
import           FileAPI.API (fileAPIProxy, FileAPI, HTTPFile(..))
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

basePath :: FilePath
basePath = "/data/files/"

readFileImpl :: Maybe String -> String -> App HTTPFile
readFileImpl maybeToken path = authenticate maybeToken $ do
  let filePath = basePath ++ path
  exists <- liftIO $ doesFileExist filePath
  if exists
  then do
    fileContents <- liftIO $ BS.readFile filePath
    pure HTTPFile { path = path, contents = unpack $ encode fileContents }
  else throwError err404

writeFileImpl :: Maybe String -> HTTPFile -> App NoContent
writeFileImpl maybeToken file = authenticate maybeToken $ do
  let filePath = basePath ++ path file
  let decodedContents = decodeLenient . pack $ contents file
  liftIO $ BS.writeFile filePath decodedContents
  pure NoContent

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
