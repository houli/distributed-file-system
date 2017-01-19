{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryAPI
  ( directoryAPIClient
  , directoryAPIProxy
  , DirectoryAPI
  ) where

import Servant
import Servant.Client (client)

import Models (File, Node)

type DirectoryAPI = "ls" :> Header "Authorization" String :> Get '[JSON] [File]
               :<|> "whereis" :> Header "Authorization" String :> Get '[JSON] Node
               :<|> "registerFileServer" :> PostNoContent '[JSON] NoContent

directoryAPIProxy :: Proxy DirectoryAPI
directoryAPIProxy = Proxy

directoryAPIClient = client directoryAPIProxy
