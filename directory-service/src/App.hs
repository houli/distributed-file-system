module App
  ( app
  ) where

import Control.Monad.Reader (ReaderT)
import Database.Persist.Postgresql
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Auth.Client
import Servant.Client

import AuthAPI (authAPIClient)
import Config (Config(..))
import DirectoryAPI (directoryAPIProxy, DirectoryAPI)
import Models

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
ls = undefined

whereis :: Maybe String -> App Node
whereis = undefined

registerFileServer :: App NoContent
registerFileServer = undefined

_ :<|> _ :<|> verifyJWT = authAPIClient
