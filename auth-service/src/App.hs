{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  ) where

import Control.Monad.Reader (liftIO, ReaderT)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (addUTCTime, getCurrentTime, NominalDiffTime)
import Database.Persist.Postgresql (entityVal, fromSqlKey, getBy, insertBy, ConnectionPool)
import Servant
import Servant.Auth.Server

import AuthAPI.API (authAPIProxy, AuthAPI, UserCreationResponse(..))
import Models (runDB, User(..), Unique(..))

-- Type alias custom monad to handle passing the Postgres connection pool around
type App = ReaderT ConnectionPool Handler

appToServer :: JWTSettings -> ConnectionPool -> Server (AuthAPI auths)
appToServer jwtSettings pool = enter (runReaderTNat pool) (server jwtSettings)

app :: JWTSettings -> ConnectionPool -> Application
app jwtSettings pool = serveWithContext authAPIProxy context (appToServer jwtSettings pool)
  where context = defaultCookieSettings :. jwtSettings :. EmptyContext

server :: JWTSettings -> ServerT (AuthAPI auths) App
server jwts = register jwts :<|> login jwts :<|> verifyJWT

register :: JWTSettings -> User -> App (Headers '[Header "Token" ByteString] UserCreationResponse)
register jwts user = do
  let hashRounds = 17 -- PKBDF1 hashing iterations
  hashedPassword <- liftIO $ makePassword (pack $ userPassword user) hashRounds
  newOrExistingUser <- runDB $ insertBy user { userPassword = unpack hashedPassword }
  case newOrExistingUser of
    Left _ -> throwError err500 { errBody = "Username already taken" }
    Right newUser -> do
      jwt <- createToken 60 user jwts
      pure $ addHeader jwt UserCreationResponse { userId = fromSqlKey newUser }

login :: JWTSettings -> User -> App (Headers '[Header "Token" ByteString] NoContent)
login jwts login = do
  maybeUser <- runDB $ getBy $ UniqueUsername (userUsername login)
  case maybeUser of
    Nothing -> loginError -- Username doesn't exist
    Just user -> if verifyPassword (pack $ userPassword login) (pack $ userPassword $ entityVal user)
                 then do
                   jwt <- createToken 60 login jwts
                   pure $ addHeader jwt NoContent
                 else loginError -- Incorrect password
  where loginError = throwError err401 { errBody = "Incorrect username or password" }

createToken :: NominalDiffTime -> User -> JWTSettings -> App ByteString
createToken expiryMinutes user jwts = do
  time <- liftIO getCurrentTime
  let expiryTime = addUTCTime (expiryMinutes * 60) time
  eitherJWT <- liftIO $ makeJWT user jwts (Just expiryTime)
  case eitherJWT of
    Left _ -> throwError err500 { errBody = "Unable to create JWT" }
    Right jwt -> pure jwt

verifyJWT :: AuthResult User -> App NoContent
verifyJWT (Authenticated _) = pure NoContent
verifyJWT _ = throwError err401
