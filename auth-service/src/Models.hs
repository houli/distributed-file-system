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
  , User(..)
  ) where

import Control.Monad.Reader (MonadIO(..), MonadReader(..))
import Database.Persist.Postgresql (runMigration, runSqlPool, ConnectionPool, SqlPersistT)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  username String
  password String
  UniqueUsername username
  deriving Show
|]

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

runDb :: (MonadReader ConnectionPool m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
  pool <- ask
  liftIO $ runSqlPool query pool
