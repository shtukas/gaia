{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ScanningAndRecordingManager (
    getCurrentMerkleRootForFSScanRoot,
    generalScan
) where

import qualified Data.Aeson                 as A
    -- JSON library
    -- A.encode :: A.ToJSON a => a -> Char8.ByteString

import qualified Data.Text                  as T
    -- T.pack :: String -> T.Text

import qualified System.Directory           as Dir
    -- doesDirectoryExist :: FilePath -> IO Bool
    -- getDirectoryContents :: FilePath -> IO [FilePath]

import           System.FilePath
    -- takeFileName :: FilePath -> FilePath
    -- type FilePath = String

import           System.Posix

import qualified Data.ByteString.Lazy.Char8 as Char8
    -- Char8.pack :: [Char] -> Char8.ByteString
    -- Char8.readFile :: FilePath -> IO Char8.ByteString

import qualified AesonObjectsUtils

import qualified FSRootsManagement

import qualified Xcache

import qualified Data.Maybe                 as M

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

locationToAesonJSONVAlueRecursivelyComputedaAndStored :: Locationpath -> IO A.Value
locationToAesonJSONVAlueRecursivelyComputedaAndStored location = do
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
    return $ AesonObjectsUtils.makeAesonValueForFile ( getLocationName filepath ) ( fromIntegral filesize ) filecontents

--{
--	"aion-type" : "directory"
--	"version"   : 1
--	"name"      : String
--	"contents"  : [Aion-Hash]
--}

folderpathToAesonJSONValue :: Folderpath -> IO A.Value
folderpathToAesonJSONValue folderpath = do
    directoryContents <- Dir.getDirectoryContents folderpath
    aesonvalues <- mapM (\filename -> locationToAesonJSONVAlueRecursivelyComputedaAndStored $ folderpath ++ "/" ++ filename)
                       (excludeDotFolders directoryContents)
    caskeys <- mapM AesonObjectsUtils.commitAesonValueToCAS aesonvalues
    let aesonvalues2 = map (A.String . T.pack) caskeys
    return $ AesonObjectsUtils.makeAesonValueForDirectory (getLocationName folderpath) aesonvalues2

-- ---------------------------------------------------------------

locationExists :: Locationpath -> IO Bool
locationExists locationpath = do
    exists1 <- Dir.doesDirectoryExist locationpath
    exists2 <- Dir.doesFileExist locationpath
    return $ exists1 || exists2

computeMerkleRootForLocationRecursivelyComputedaAndStored :: Locationpath -> IO ( Maybe String )
computeMerkleRootForLocationRecursivelyComputedaAndStored locationpath = do
    exists <- locationExists locationpath
    if exists
        then do
            value <- locationToAesonJSONVAlueRecursivelyComputedaAndStored locationpath
            string <- AesonObjectsUtils.commitAesonValueToCAS value
            return $ Just string
        else
            return Nothing

commitMerkleRootForFSScanRoot :: String -> String -> IO ()
commitMerkleRootForFSScanRoot fsscanlocationpath merkleroot = do
    Xcache.set (FSRootsManagement.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan fsscanlocationpath) merkleroot

getCurrentMerkleRootForFSScanRoot :: String -> IO ( Maybe String )
getCurrentMerkleRootForFSScanRoot locationpath = Xcache.get (FSRootsManagement.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath)

-- ---------------------------------------------------------------

generalScan :: IO ()
generalScan = do
    scanroots <- FSRootsManagement.getFSScanRoots
    _ <- sequence $ map (\scanroot -> 
                            do
                                s1 <- computeMerkleRootForLocationRecursivelyComputedaAndStored scanroot 
                                -- s1: Maybe String
                                if M.isJust s1 
                                    then do
                                        putStrLn $ "location: "++scanroot
                                        putStrLn $ "merkle  : "++(M.fromJust s1) 
                                        commitMerkleRootForFSScanRoot scanroot (M.fromJust s1)
                                    else do
                                        return ()
                         ) scanroots
    return ()


