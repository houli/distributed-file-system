module FileAPI.Client
  ( fileAPIClient
  ) where

import Servant.Client (client)

import FileAPI.API (fileAPIProxy)

fileAPIClient = client fileAPIProxy
