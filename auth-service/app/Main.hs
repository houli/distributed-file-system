{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Logger (runStdoutLoggingT)
import           Data.Aeson (decodeStrict)
import qualified Data.ByteString as BS
import           Database.Persist.Postgresql (createPostgresqlPool, runSqlPool, ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.Auth.Server (defaultJWTSettings)

import           App (app)
import           Models (runMigrations)

connStr :: ConnectionString
connStr = "host=db port=5432 dbname=dfs user=postgres"

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  runSqlPool runMigrations pool
  jwkContents <- BS.readFile "/data/secrets/jwk.json"
  case decodeStrict jwkContents of
    Nothing -> error "Unable to decode JWK"
    Just jwk -> run 8080 $ logStdoutDev $ app (defaultJWTSettings jwk) pool
