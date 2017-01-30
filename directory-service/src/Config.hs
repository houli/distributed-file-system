module Config
  ( Config(..)
  ) where

import Database.Persist.Postgresql (ConnectionPool)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)

import Models (HasConnectionPool(..))

-- Type used to pass around shared configuration necessary to run this service
data Config = Config
  { pool :: ConnectionPool
  , manager :: Manager
  , authBase :: BaseUrl
  }

-- Allow our runDB function to pull the DB pool from our config
instance HasConnectionPool Config where
  connectionPool = pool
