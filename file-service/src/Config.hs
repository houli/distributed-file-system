module Config where

import Control.Monad.Reader
import Database.Persist.Postgresql (ConnectionPool)
import GHC.Int (Int64)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)

import Models

data Config = Config
  { pool :: ConnectionPool
  , serverId :: Int64
  , manager :: Manager
  , authBase :: BaseUrl
  }

instance HasConnectionPool Config where
  connectionPool = pool