{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module FileAPI.API
  ( fileAPIProxy
  , FileAPI
  , HTTPFile(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

import AuthAPI.API (AuthToken)

type FileAPI = "readFile" :> -- Endpoint to read from a file on a node
               AuthToken :>
               ReqBody '[JSON] FilePath :> -- File path being read
               Get '[JSON] HTTPFile -- base64 encoded file contents
          :<|> "writeFile" :> -- Endpoint to write to a file on a node
               AuthToken :>
               ReqBody '[JSON] HTTPFile :> -- Path and base64 encoded file contents
               PostNoContent '[JSON] NoContent -- No content but no error HTTP status signifies success
          :<|> "replicate" :> -- Endpoint to receive a request to replicate from a primary
               ReqBody '[JSON] HTTPFile :> -- Path and base64 encoded contents of file to be replicated
               PostNoContent '[JSON] NoContent -- No content but no error HTTP status signifies success

data HTTPFile = HTTPFile
  { path :: FilePath
  , contents :: String
  } deriving (Generic, FromJSON, ToJSON)

fileAPIProxy :: Proxy FileAPI
fileAPIProxy = Proxy
