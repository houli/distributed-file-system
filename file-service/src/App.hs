{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader (asks, liftIO, MonadIO, ReaderT)
import qualified Data.ByteString as BS
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.ByteString.Char8 (pack, unpack)
import           Database.Persist.Postgresql ((=.), entityVal, getBy, insert, ConnectionPool)
import           Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import           Servant
import           Servant.Auth.Client (Token(..))
import           Servant.Client (parseBaseUrl)
import           System.Directory (doesFileExist)
import           System.Environment (getEnv)
import           System.Random (randomRIO)

import           AuthAPI.Client (authAPIClient)
import           Config (Config(..))
import           FileAPI.API (fileAPIProxy, FileAPI, HTTPFile(..))
import           FileAPI.Client (fileAPIClient)
import           Models (runDB, EntityField(..), File(..), NodeId, Unique(..))

type App = ReaderT Config Handler

appToServer :: Config -> Server FileAPI
appToServer cfg = enter (runReaderTNat cfg) server

app :: ConnectionPool -> NodeId -> IO Application
app pool nodeId = do
  manager <- newManager defaultManagerSettings
  authBase <- parseBaseUrl "http://auth-service:8080"
  pure $ serve fileAPIProxy (appToServer $ Config pool nodeId manager authBase)

server :: ServerT FileAPI App
server = readFileImpl :<|> writeFileImpl :<|> replicateImpl

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
  nodeId <- asks nodeId
  maybeFile <- runDB $ getBy $ UniquePath (path file) -- See if file exists already
  case maybeFile of
    Nothing -> do
      runDB $ insert (File (path file) nodeId) -- Doesn't exist, create it and write to disk
      writeAndReplicate file
    Just dbFile ->
      if fileNode (entityVal dbFile) == nodeId -- Check if locked by another primary
      then writeAndReplicate file -- Not locked on another primary, can write
      else throwError err500 { errBody = "File is locked to another primary node" }

writeAndReplicate :: HTTPFile -> App NoContent
writeAndReplicate file = do
    writeToDisk file
    manager <- asks manager
    liftIO $ forkIO $ doReplicateRequest manager file -- Run replicate request asynchronously in another thread
    pure NoContent

doReplicateRequest :: MonadIO m => Manager -> HTTPFile -> m ()
doReplicateRequest manager file = liftIO $ do
  exposedPort <- read <$> getEnv "EXPOSED_PORT"
  server <- snd <$> pick (filter ((exposedPort /=) . fst) servers) -- Pick a random server other than this one
  fsBase <- parseBaseUrl server
  runExceptT $ replicateClient file manager fsBase -- Send replication request to the other node
  pure ()
  where pick xs = (xs !!) <$> randomRIO (0, length xs - 1)
        servers = [ (8081, "http://file-service1:8080")
                  , (8082, "http://file-service2:8080")
                  , (8083, "http://file-service3:8080")
                  ]

replicateImpl :: HTTPFile -> App NoContent
replicateImpl file = writeToDisk file >> pure NoContent

writeToDisk :: MonadIO m => HTTPFile -> m ()
writeToDisk file = do
  let filePath = basePath ++ path file
  let decodedContents = decodeLenient . pack $ contents file
  liftIO $ BS.writeFile filePath decodedContents

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

-- Pattern match out the routes of the FileAPI
_ :<|> _ :<|> replicateClient = fileAPIClient
