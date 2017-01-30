module Main where

import           Control.Exception (bracket)
import           Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Char8 as BS
import           Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import           Servant
import           Servant.Client (parseBaseUrl, BaseUrl)
import           System.Fuse (defaultExceptionHandler, fuseMain)
import           System.IO

import           AuthAPI.Client (authAPIClient)
import           Config (Config(..))
import           FUSEOps (dfsOps)
import           Models (User(..))

-- Pattern match out routes we need from our Servant APIs
_ :<|> authLogin :<|> _ = authAPIClient

authService :: IO BaseUrl
authService = parseBaseUrl "http://localhost:8080"

login :: User -> Manager -> IO String
login user manager = do
  authBase <- authService
  loginResponse <- runExceptT $ authLogin user manager authBase
  case loginResponse of
    Left _ -> error "Failed to login"
    Right success -> pure $ head $ map (BS.unpack . snd) (getHeaders success)

main :: IO ()
main = do
  username <- putStr "Username: " *> hFlush stdout *> getLine
  password <- putStr "Password: " *> hFlush stdout *> withEcho False getLine <* putChar '\n' -- Don't display password characters
  manager <- newManager defaultManagerSettings
  token <- login (User username password) manager
  fuseMain (dfsOps (Config manager token)) defaultExceptionHandler

-- Run action with echoing on or off
withEcho :: Bool -> IO a -> IO a
withEcho echo action =
  bracket (hGetEcho stdin)
          (hSetEcho stdin)
          (const $ hSetEcho stdin echo >> action)
