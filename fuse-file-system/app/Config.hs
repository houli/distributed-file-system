module Config
  ( Config(..)
  ) where

import Network.HTTP.Client (Manager)

data Config = Config Manager String
