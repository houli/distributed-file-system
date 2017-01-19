module Config
  ( Config(..)
  ) where

import Database.Persist.Postgresql (ConnectionPool)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)

import Models (HasConnectionPool(..))

data Config = Config
  { pool :: ConnectionPool
  , manager :: Manager
  , authBase :: BaseUrl
  }

instance HasConnectionPool Config where
  connectionPool = pool
