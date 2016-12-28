{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, runSqlPool)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send)

import Models (connStr, runMigrations)

-- 1. Run migrations
-- 2. Run simple TCP server to satisfy services waiting on migrations
main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 5
  runSqlPool runMigrations pool
  withSocketsDo $ do
    (serveraddr : _) <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] })) Nothing (Just "8000")
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 5
    loop sock

loop :: Socket -> IO ()
loop sock = do
  (clientSock, _) <- accept sock
  forkIO $ respond clientSock
  loop sock

respond :: Socket -> IO ()
respond sock = do
  send sock "OK\n"
  close sock
