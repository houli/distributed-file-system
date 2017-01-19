module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import App (app)
import Models (connStr)

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  application <- app pool
  run 8080 $ logStdoutDev application
