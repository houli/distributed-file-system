{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger                 (runStdoutLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import App
import Models

connStr :: ConnectionString
connStr = "host=db port=5432 dbname=dfs user=postgres"

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  runSqlPool runMigrations pool
  run 8080 $ logStdoutDev $ app pool
