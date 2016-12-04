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
  ( runDb
  , runMigrations
  , User
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Data.Text
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  username Text
  UniqueUsername username
  deriving Show
|]

connectionString :: ConnectionString
connectionString = "host=db port=5432 dbname=dfs user=postgresql"

runMigrations :: SqlPersistT IO ()
runMigrations = runMigration migrateAll

runDb :: (MonadReader ConnectionPool m, MonadIO m) => SqlPersistT IO a -> m a
runDb query = do
    pool <- ask
    liftIO $ runSqlPool query pool
