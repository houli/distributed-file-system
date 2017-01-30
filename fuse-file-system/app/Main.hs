module Main where

import           Control.Monad.Except (runExceptT)
import           Data.ByteString.Base64 (decodeLenient, encode)
import qualified Data.ByteString.Char8 as BS
import           Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import           Servant
import           Servant.Client (parseBaseUrl, BaseUrl)
import           System.Fuse
import           System.Posix.Files hiding (fileSize)
import           System.Posix.IO
import           System.Posix.Types (ByteCount, FileOffset)

import           AuthAPI.Client (authAPIClient)
import           DirectoryAPI.Client (directoryAPIClient)
import           FileAPI.API (HTTPFile(..))
import           FileAPI.Client (fileAPIClient)
import           Models

-- Pattern match out routes we need from our Servant APIs
_ :<|> authLogin :<|> _                    = authAPIClient
ls :<|> whereis :<|> roundRobinNode :<|> _ = directoryAPIClient
readFileClient :<|> writeFileClient :<|> _ = fileAPIClient

data Config = Config Manager String

localhostBase :: String
localhostBase = "http://localhost:"

authService :: IO BaseUrl
authService = parseBaseUrl "http://localhost:8080"

directoryService :: IO BaseUrl
directoryService = parseBaseUrl "http://localhost:8084"

dfsOps :: Config -> FuseOperations ()
dfsOps config = defaultFuseOps { fuseGetFileStat        = dfsGetFileStat config
                               , fuseOpen               = dfsOpen config
                               , fuseRead               = dfsRead config
                               , fuseOpenDirectory      = dfsOpenDirectory
                               , fuseReadDirectory      = dfsReadDirectory config
                               , fuseGetFileSystemStats = dfsGetFileSystemStats
                               }

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerWriteMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupWriteMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherWriteMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat :: Int -> FuseContext -> FileStat
fileStat size ctx = FileStat { statEntryType = RegularFile
                             , statFileMode = foldr1 unionFileModes
                                                [ ownerReadMode
                                                , ownerWriteMode
                                                , groupReadMode
                                                , groupWriteMode
                                                , otherReadMode
                                                , otherWriteMode
                                                ]
                             , statLinkCount = 1
                             , statFileOwner = fuseCtxUserID ctx
                             , statFileGroup = fuseCtxGroupID ctx
                             , statSpecialDeviceID = 0
                             , statFileSize = fromIntegral size
                             , statBlocks = 1
                             , statAccessTime = 0
                             , statModificationTime = 0
                             , statStatusChangeTime = 0
                             }

lookupNode :: Config -> FilePath -> IO (Maybe Node)
lookupNode (Config manager token) path = do
  directoryBase <- directoryService
  nodeResponse <- runExceptT $ whereis (Just token) path manager directoryBase
  case nodeResponse of
    Left _ -> pure Nothing
    Right node -> pure $ Just node

lookupFile :: Config -> FilePath -> IO (Maybe HTTPFile)
lookupFile config@(Config manager token) path = do
  maybeNode <- lookupNode config path
  case maybeNode of
    Nothing -> pure Nothing
    Just node -> do
      fsBase <- parseBaseUrl $ localhostBase ++ (show $ nodePort node)
      fileResponse <- runExceptT $ readFileClient (Just token) path manager fsBase
      case fileResponse of
        Left _ -> pure Nothing
        Right file -> pure $ Just file

dfsGetFileStat :: Config -> FilePath -> IO (Either Errno FileStat)
dfsGetFileStat _ "/" = do
  ctx <- getFuseContext
  pure $ Right $ dirStat ctx
dfsGetFileStat config path = do
  maybeFile <- lookupFile config path
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just file -> do
      ctx <- getFuseContext
      pure $ Right $ fileStat (BS.length $ decodeLenient $ BS.pack $ contents file) ctx

dfsOpen :: Config -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
dfsOpen config path _ _ = do
  maybeFile <- lookupFile config path
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just _ -> pure $ Right ()

dfsRead :: Config -> FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
dfsRead config path _ byteCount offset = do
  maybeFile <- lookupFile config path
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just file -> pure . Right . seekContents . decodeLenient . BS.pack $ contents file
  where
    seekContents contents = BS.take (fromIntegral byteCount) $ BS.drop (fromIntegral offset) contents

dfsOpenDirectory :: FilePath -> IO Errno
dfsOpenDirectory "/" = pure eOK
dfsOpenDirectory _   = pure eNOENT

dfsReadDirectory :: Config -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
dfsReadDirectory (Config manager token) "/" = do
  ctx <- getFuseContext
  directoryBase <- directoryService
  lsResponse <- runExceptT $ ls (Just token) manager directoryBase
  case lsResponse of
    Left _ -> pure $ Left eNOENT
    Right files -> pure $ Right $ [(".", dirStat ctx), ("..", dirStat ctx)] ++ pathStatPairs files ctx
  where
    pathStatPairs [] _ = []
    pathStatPairs (x:xs) ctx = (filePath x, fileStat (fileSize x) ctx) : pathStatPairs xs ctx
dfsReadDirectory _ _ = pure $ Left eNOENT

dfsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
dfsGetFileSystemStats _ =
  pure $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

login :: User -> Manager -> IO String
login user manager = do
  authBase <- authService
  loginResponse <- runExceptT $ authLogin user manager authBase
  case loginResponse of
    Left _ -> error "Failed to login"
    Right success -> pure $ head $ map (BS.unpack . snd) (getHeaders success)

main :: IO ()
main = do
  username <- getLine
  password <- getLine
  manager <- newManager defaultManagerSettings
  token <- login (User username password) manager
  fuseMain (dfsOps (Config manager token)) defaultExceptionHandler
