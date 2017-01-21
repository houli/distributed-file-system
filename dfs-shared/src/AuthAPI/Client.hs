module AuthAPI.Client
  ( authAPIClient
  ) where

import Servant.Client (client)

import AuthAPI.API (authAPIProxy)

authAPIClient = client authAPIProxy
