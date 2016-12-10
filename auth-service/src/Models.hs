{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models
  ( runDb
  , runMigrations
  , Unique(..)
  , User(..)
  ) where

import Control.Monad.Reader (MonadIO(..), MonadReader(..))
import Database.Persist.Postgresql (runMigration, runSqlPool, ConnectionPool, SqlPersistT, Unique)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Servant.Auth.Server

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

runDb :: (MonadReader ConnectionPool m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
  pool <- ask
  liftIO $ runSqlPool query pool
