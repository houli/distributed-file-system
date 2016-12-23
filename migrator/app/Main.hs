module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, runSqlPool)

import Models (connStr, runMigrations)

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  runSqlPool runMigrations pool
