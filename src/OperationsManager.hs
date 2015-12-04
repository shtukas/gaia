{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module OperationsManager (
    computeMerkleRootForLocation,
    locationToAeonJSONVAlue,
    aeonJSONVAlueToString
) where

import ContentAddressableStore

import qualified Data.Aeson as A
    -- JSON library
    -- A.encode :: A.ToJSON a => a -> Char8.ByteString

import qualified GHC.Exts as E 
    -- support for the JSON library

import qualified System.Directory as Dir
    -- doesDirectoryExist :: FilePath -> IO Bool
    -- getDirectoryContents :: FilePath -> IO [FilePath]

import qualified Data.Text as T
    -- T.pack :: String -> T.Text

import System.Posix

import System.Posix.Files
    -- fileSize :: FileStatus -> FileOffset
    -- getFileStatus :: FilePath -> IO FileStatus

import System.FilePath.Posix
    -- takeFileName :: FilePath -> FilePath
    -- type FilePath = String

import Data.Scientific

import qualified Data.ByteString.Lazy.Char8 as Char8
    -- Char8.pack :: [Char] -> Char8.ByteString
    -- Char8.readFile :: FilePath -> IO Char8.ByteString

import Data.Digest.Pure.SHA as SHA
    -- SHA.sha1 :: Char8.ByteString -> Digest SHA1State
    -- SHA.showDigest :: Digest t -> String

import Data.Vector as V

import qualified System.Directory as Dir

type Filepath = String
type Folderpath = String
type Locationpath = String

-- ---------------------------------------------------------------

getLocationName :: Locationpath -> Locationpath
getLocationName location = takeFileName location

getFileSize :: Filepath -> IO FileOffset
getFileSize filepath = do
    stat <- getFileStatus filepath
    return (fileSize stat)

getFileContents :: Filepath -> IO Char8.ByteString
getFileContents filepath = do
    contents <- Char8.readFile filepath
    return contents

excludeDotFolders :: [FilePath] -> [FilePath]
excludeDotFolders list = Prelude.filter (\filename -> ( Prelude.head filename )/='.' ) list 

-- ---------------------------------------------------------------

aeonJSONVAlueToString :: A.Value -> String
aeonJSONVAlueToString value = Char8.unpack $ A.encode $ value

commitAeonJSONValueToCAS :: A.Value -> IO String
commitAeonJSONValueToCAS value = ContentAddressableStore.set $ aeonJSONVAlueToString value

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
    return $ A.Object $ E.fromList [
        ("aion-type" , A.String "file"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack $ getLocationName filepath),
        ("size"      , A.Number $ scientific ( fromIntegral filesize ) 1 ),
        ("hash"      , A.String $ T.pack $ SHA.showDigest $ SHA.sha1 $ filecontents ) ]

--{
--	"aion-type" : "directory"
--	"version"   : 1
--	"name"      : String
--	"contents"  : [Aion-Hash]
--}

folderpathToAesonJSONValue :: Folderpath -> IO A.Value
folderpathToAesonJSONValue folderpath = do
    directoryContents <- Dir.getDirectoryContents folderpath
    aeonvalues <- Prelude.sequence $ Prelude.map (\filename -> locationToAeonJSONVAlue $ folderpath Prelude.++ "/" Prelude.++ filename ) ( excludeDotFolders directoryContents )
    caskeys <- Prelude.sequence $ Prelude.map (\aeonvalue -> commitAeonJSONValueToCAS aeonvalue ) aeonvalues
    let aeonvalues2 = Prelude.map (\caskey -> A.String $ T.pack caskey ) caskeys
    return $ A.Object $ E.fromList [
        ("aion-type" , A.String "directory"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack $ getLocationName folderpath),
        ("contents"  , A.Array $ V.fromList $ aeonvalues2 ) ]

-- ---------------------------------------------------------------

-- Do not use. Implementation not finished.
computeMerkleRootForLocation :: Locationpath -> IO [String]
computeMerkleRootForLocation locationpath = do
    exists <- Dir.doesDirectoryExist locationpath
    if exists 
        then do
            -- Here we have a Directory
            contents <- Dir.getDirectoryContents locationpath
            return contents
        else do
            -- Here we have a File
            return [""]



