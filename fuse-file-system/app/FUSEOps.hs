module FUSEOps
  ( dfsOps
  ) where

import           Control.Monad.Except (runExceptT)
import           Data.ByteString.Base64 (decodeLenient, encode)
import qualified Data.ByteString.Char8 as BS
import           Foreign.C.Types (CSize(..))
import           Servant
import           Servant.Client (parseBaseUrl, BaseUrl)
import           System.Fuse
import           System.Posix.Files hiding (fileSize)
import           System.Posix.IO
import           System.Posix.Types (ByteCount, DeviceID, FileMode, FileOffset)

import           Config (Config(..))
import           DirectoryAPI.Client (directoryAPIClient)
import           FileAPI.API (HTTPFile(..))
import           FileAPI.Client (fileAPIClient)
import           Models

-- Pattern match out routes we need from our Servant APIs
ls :<|> whereis :<|> roundRobinNode :<|> _ = directoryAPIClient
readFileClient :<|> writeFileClient :<|> _ = fileAPIClient

localhostBase :: String
localhostBase = "http://localhost:"

directoryService :: IO BaseUrl
directoryService = parseBaseUrl "http://localhost:8084"

dfsOps :: Config -> FuseOperations ()
dfsOps config = defaultFuseOps { fuseGetFileStat        = dfsGetFileStat config
                               , fuseOpen               = dfsOpen config
                               , fuseRead               = dfsRead config
                               , fuseWrite              = dfsWrite config
                               , fuseCreateDevice       = dfsCreateDevice config
                               , fuseOpenDirectory      = dfsOpenDirectory
                               , fuseReadDirectory      = dfsReadDirectory config
                               , fuseGetFileSystemStats = dfsGetFileSystemStats
                               , fuseSetFileSize        = \ _ _ -> pure eOK
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

filterSlash :: FilePath -> FilePath
filterSlash = filter ('/' /=)

lookupNode :: Config -> FilePath -> IO (Maybe Node)
lookupNode (Config manager token) path = do
  directoryBase <- directoryService
  nodeResponse <- runExceptT $ whereis (Just token) path manager directoryBase
  case nodeResponse of
    Left _ -> pure Nothing
    Right node -> pure $ Just node

lookupFile :: Config -> FilePath -> IO (Maybe HTTPFile)
lookupFile config@(Config manager token) path = do
  maybeNode <- lookupNode config (filterSlash path)
  case maybeNode of
    Nothing -> pure Nothing
    Just node -> do
      fsBase <- parseBaseUrl $ localhostBase ++ (show $ nodePort node) -- Build up file service address
      fileResponse <- runExceptT $ readFileClient (Just token) (filterSlash path) manager fsBase
      case fileResponse of
        Left _ -> pure Nothing
        Right file -> pure $ Just file

dfsGetFileStat :: Config -> FilePath -> IO (Either Errno FileStat)
dfsGetFileStat _ "/" = do
  ctx <- getFuseContext
  pure $ Right $ dirStat ctx
dfsGetFileStat config path = do
  maybeFile <- lookupFile config (filterSlash path)
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just file -> do
      ctx <- getFuseContext
      pure $ Right $ fileStat (BS.length $ decodeLenient $ BS.pack $ contents file) ctx

dfsOpen :: Config -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
dfsOpen config path _ _ = do
  maybeFile <- lookupFile config (filterSlash path)
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just _ -> pure $ Right ()

dfsRead :: Config -> FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno BS.ByteString)
dfsRead config path _ byteCount offset = do
  maybeFile <- lookupFile config (filterSlash path)
  case maybeFile of
    Nothing -> pure $ Left eNOENT
    Just file -> pure . Right . seekContents . decodeLenient . BS.pack $ contents file
  where
    seekContents contents = BS.take (fromIntegral byteCount) $ BS.drop (fromIntegral offset) contents

dfsWrite :: Config -> FilePath -> () -> BS.ByteString -> FileOffset -> IO (Either Errno ByteCount)
dfsWrite (Config manager token) path _ buffer _ = do
  directoryBase <- directoryService
  rrNodeResponse <- runExceptT $ roundRobinNode (Just token) (filterSlash path) manager directoryBase
  case rrNodeResponse of
    Left _ -> pure $ Left eACCES
    Right node -> do
      fsBase <- parseBaseUrl $ localhostBase ++ (show $ nodePort node) -- Build up file service address
      let base64Contents = BS.unpack $ encode buffer
      writeResponse <- runExceptT $ writeFileClient (Just token) (HTTPFile (filterSlash path) base64Contents) manager fsBase
      case writeResponse of
        Left _ -> pure $ Left eACCES
        Right _ -> do
          let bufferLength = CSize $ fromIntegral $ BS.length buffer
          pure $ Right bufferLength

dfsCreateDevice :: Config -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
dfsCreateDevice config@(Config manager token) path _ _ _ = do
  maybeNode <- lookupNode config (filterSlash path)
  case maybeNode of
    Nothing -> do
      directoryBase <- directoryService
      rrNodeResponse <- runExceptT $ roundRobinNode (Just token) (filterSlash path) manager directoryBase
      case rrNodeResponse of
        Left _ -> pure eACCES
        Right node -> do
          fsBase <- parseBaseUrl $ localhostBase ++ (show $ nodePort node) -- Build up file service address
          writeResponse <- runExceptT $ writeFileClient (Just token) (HTTPFile (filterSlash path) "") manager fsBase
          case writeResponse of
            Left _ -> pure eACCES
            Right _ -> pure eOK
    Just _ -> pure eEXIST

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
    , fsStatBlocksFree = 1000000
    , fsStatBlocksAvailable = 1000000
    , fsStatFileCount = 5
    , fsStatFilesFree = 1000000
    , fsStatMaxNameLength = 255
    }
