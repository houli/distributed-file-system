{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module FileAPI
  ( fileAPIClient
  , fileAPIProxy
  , FileAPI
  , FileUpload(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Client (client)
import System.IO (FilePath)

type FileAPI = "readFile" :> Header "Authorization" String :> Capture "path" FilePath :> Get '[JSON] NoContent
          :<|> "writeFile" :> Header "Authorization" String :> ReqBody '[JSON] FileUpload :> PostNoContent '[JSON] NoContent

data FileUpload = FileUpload
  { path :: FilePath
  , contents :: String
  } deriving (Generic, FromJSON, ToJSON)

fileAPIProxy :: Proxy FileAPI
fileAPIProxy = Proxy

fileAPIClient = client fileAPIProxy
