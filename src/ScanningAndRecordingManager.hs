{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ScanningAndRecordingManager (
    cycleMerkleRootComputationForLocation
) where

import           ContentAddressableStore
import           Control.Monad              (unless)
import qualified Data.Aeson                 as A
    -- JSON library
    -- A.encode :: A.ToJSON a => a -> Char8.ByteString

import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                  as T
    -- T.pack :: String -> T.Text

import qualified Data.Vector                as V
import qualified GHC.Exts                   as E
    -- support for the JSON library

import qualified System.Directory           as Dir
    -- doesDirectoryExist :: FilePath -> IO Bool
    -- getDirectoryContents :: FilePath -> IO [FilePath]

import           System.FilePath
    -- takeFileName :: FilePath -> FilePath
    -- type FilePath = String

import           System.Posix
import           System.Posix.Files
    -- fileSize :: FileStatus -> FileOffset
    -- getFileStatus :: FilePath -> IO FileStatus

import qualified Data.ByteString.Lazy.Char8 as Char8
    -- Char8.pack :: [Char] -> Char8.ByteString
    -- Char8.readFile :: FilePath -> IO Char8.ByteString

import           Data.Digest.Pure.SHA       as SHA
    -- SHA.sha1 :: Char8.ByteString -> Digest SHA1State
    -- SHA.showDigest :: Digest t -> String

import qualified AeonObjectsUtils

type Filepath = String
type Folderpath = String
type Locationpath = String

-- ---------------------------------------------------------------

getLocationName :: Locationpath -> Locationpath
getLocationName = takeFileName

getFileSize :: Filepath -> IO FileOffset
getFileSize filepath = do
    stat <- getFileStatus filepath
    return (fileSize stat)

getFileContents :: Filepath -> IO Char8.ByteString
getFileContents = Char8.readFile

excludeDotFolders :: [FilePath] -> [FilePath]
excludeDotFolders = filter (\filename -> head filename /= '.' )

-- ---------------------------------------------------------------

locationToAeonJSONVAlue :: Locationpath -> IO A.Value
locationToAeonJSONVAlue location = do
    isFile      <- Dir.doesFileExist location
    isDirectory <- Dir.doesDirectoryExist location
    if isFile
        then filepathToAesonJSONValue ( location :: Filepath )
        else if isDirectory
            then folderpathToAesonJSONValue ( location :: Folderpath )
            else return A.Null

--{
--	"aion-type" : "file"
--	"version"   : 1
--	"name"      : String
--	"size"      : Integer
--	"hash"      : sha1-hash
--}

filepathToAesonJSONValue :: Filepath -> IO A.Value
filepathToAesonJSONValue filepath = do
    filesize <- getFileSize filepath
    filecontents <- getFileContents filepath
    return $ AeonObjectsUtils.makeAeonJSONValueForFile ( getLocationName filepath ) ( fromIntegral filesize ) filecontents

--{
--	"aion-type" : "directory"
--	"version"   : 1
--	"name"      : String
--	"contents"  : [Aion-Hash]
--}

folderpathToAesonJSONValue :: Folderpath -> IO A.Value
folderpathToAesonJSONValue folderpath = do
    directoryContents <- Dir.getDirectoryContents folderpath
    aeonvalues <- mapM (\filename -> locationToAeonJSONVAlue $ folderpath ++ "/" ++ filename)
                       (excludeDotFolders directoryContents)
    caskeys <- mapM AeonObjectsUtils.commitAeonJSONValueToCAS aeonvalues
    let aeonvalues2 = map (A.String . T.pack) caskeys
    return $ AeonObjectsUtils.makeAeonJSONValueForDirectory (getLocationName folderpath) aeonvalues2

-- ---------------------------------------------------------------

locationExists :: Locationpath -> IO Bool
locationExists locationpath = do
    exists1 <- Dir.doesDirectoryExist locationpath
    exists2 <- Dir.doesFileExist locationpath
    return $ exists1 || exists2

computeMerkleRootForLocation :: Locationpath -> IO ( Maybe String )
computeMerkleRootForLocation locationpath = do
    exists <- locationExists locationpath
    if exists
        then do
            value <- locationToAeonJSONVAlue locationpath
            string <- AeonObjectsUtils.commitAeonJSONValueToCAS value
            return $ Just string
        else
            return Nothing

-- In this version we scan one location, this will change later.

commitMerkleRootToUserAppData :: String -> IO ()
commitMerkleRootToUserAppData root =
    do
        folderpath <- Dir.getAppUserDataDirectory "gaia"
        Dir.createDirectoryIfMissing True folderpath
        let filepath = folderpath ++ "/" ++ "merkleroot"
        writeFile filepath root

cycleMerkleRootComputationForLocation :: String -> IO ()
cycleMerkleRootComputationForLocation location =
    do
        root <- computeMerkleRootForLocation location
        unless (fromMaybe "" root == "") $ commitMerkleRootToUserAppData $ fromMaybe "" root
