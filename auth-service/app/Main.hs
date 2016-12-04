module Main where

import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Lib

main :: IO ()
main = run 8080 $ logStdoutDev app
