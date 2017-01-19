{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module FileAPI
  ( fileAPIClient
  , fileAPIProxy
  , FileAPI
  , HTTPFile(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Client (client)

type FileAPI = "readFile" :> Header "Authorization" String :> Capture "path" FilePath :> Get '[JSON] HTTPFile
          :<|> "writeFile" :> Header "Authorization" String :> ReqBody '[JSON] HTTPFile :> PostNoContent '[JSON] NoContent

data HTTPFile = HTTPFile
  { path :: FilePath
  , contents :: String
  } deriving (Generic, FromJSON, ToJSON)

fileAPIProxy :: Proxy FileAPI
fileAPIProxy = Proxy

fileAPIClient = client fileAPIProxy
