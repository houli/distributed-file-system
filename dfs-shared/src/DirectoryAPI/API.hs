{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryAPI.API
  ( directoryAPIProxy
  , DirectoryAPI
  ) where

import Servant

import Models (File, Node)

type DirectoryAPI = "ls" :> Header "Authorization" String :> Get '[JSON] [File]
               :<|> "whereis" :> Header "Authorization" String :> ReqBody '[JSON] FilePath :> Post '[JSON] Node
               :<|> "registerFileServer" :> PostNoContent '[JSON] NoContent

directoryAPIProxy :: Proxy DirectoryAPI
directoryAPIProxy = Proxy
