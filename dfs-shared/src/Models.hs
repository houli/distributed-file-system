{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models
  ( connStr
  , runDB
  , runMigrations
  , EntityField(..)
  , File(..)
  , FileId
  , HasConnectionPool(..)
  , Node(..)
  , NodeId
  , Unique(..)
  , User(..)
  , UserId
  ) where

import Control.Monad.Reader (asks, MonadIO(..), MonadReader(..))
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql (runMigration, runSqlPool, ConnectionPool, ConnectionString, EntityField, SqlPersistT, Unique)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)

-- Declare shared database models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  username String
  password String
  UniqueUsername username
  deriving Show Generic

Node json
  port Int
  storedAt UTCTime default=now()
  UniquePort port
  deriving Show Generic

File json
  path FilePath
  node NodeId
  size Int
  UniquePath path
  deriving Show Generic
|]

-- Some instances to work with JSON Web Tokens
instance ToJWT User
instance FromJWT User

-- The class of types that can give you a database pool
class HasConnectionPool a where
  connectionPool :: a -> ConnectionPool

instance HasConnectionPool ConnectionPool where
  connectionPool = id

-- Run an arbitrary persistent query using a pool of database connections
runDB :: HasConnectionPool a => (MonadReader a m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = do
  pool <- asks connectionPool
  liftIO $ runSqlPool query pool

-- All of our shared data model migrations
runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

-- The database connection string of our postgres container
connStr :: ConnectionString
connStr = "host=db port=5432 dbname=dfs user=postgres"
