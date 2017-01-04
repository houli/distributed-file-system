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
  , HasConnectionPool(..)
  , Unique(..)
  , User(..)
  ) where

import Control.Monad.Reader (MonadIO(..), MonadReader(..))
import Database.Persist.Postgresql (runMigration, runSqlPool, ConnectionPool, ConnectionString, SqlPersistT, Unique)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)

-- Declare shared data models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  username String
  password String
  UniqueUsername username
  deriving Show Generic
|]
-- Some instances to work with JSON Web Tokens
instance ToJWT User
instance FromJWT User

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

class HasConnectionPool a where
  connectionPool :: a -> ConnectionPool

instance HasConnectionPool ConnectionPool where
  connectionPool = id

runDB :: (MonadReader ConnectionPool m, MonadIO m) => SqlPersistT IO a -> m a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool

connStr :: ConnectionString
connStr = "host=db port=5432 dbname=dfs user=postgres"
