{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module DirectoryAPI.API
  ( directoryAPIProxy
  , DirectoryAPI
  ) where

import Servant

import AuthAPI.API (AuthToken)
import Models (File, Node, NodeId)

type DirectoryAPI = "ls" :> -- List all files
                    AuthToken :>
                    Get '[JSON] [File] -- Listing of all files

               :<|> "whereis" :> -- Lookup the node for a given file path
                    AuthToken :>
                    ReqBody '[JSON] FilePath :> -- Path of file being looked up
                    Post '[JSON] Node -- Node where the file is kept

               :<|> "roundRobinNode" :> -- Next node to use as a file primary
                    AuthToken :>
                    ReqBody '[JSON] FilePath :> -- Path of file that will be written
                    Get '[JSON] Node -- Primary node of the file being stored

               :<|> "registerFileServer" :> -- Register a node with the directory service
                    ReqBody '[JSON] Int :> -- Port file server node is running on
                    Post '[JSON] NodeId -- Id of the newly created node record

directoryAPIProxy :: Proxy DirectoryAPI
directoryAPIProxy = Proxy
