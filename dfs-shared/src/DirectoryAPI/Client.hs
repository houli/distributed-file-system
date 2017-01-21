module DirectoryAPI.Client
  ( directoryAPIClient
  ) where

import Servant.Client (client)

import DirectoryAPI.API (directoryAPIProxy)

directoryAPIClient = client directoryAPIProxy
