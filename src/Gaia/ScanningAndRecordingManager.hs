{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.ScanningAndRecordingManager (
    getCurrentMerkleRootForFSScanRoot,
    generalScan
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text as T
import qualified Gaia.AionPointAbstractionUtils as GAOU
import qualified Gaia.FSRootsManagement as FSRM
import           Gaia.Types
import qualified PStorageServices.Xcache as X
import qualified System.Directory as Dir
import           System.FilePath
import           System.Posix               (FileOffset, fileSize,
                                             getFileStatus)

-- ---------------------------------------------------------------

getLocationName :: LocationPath -> LocationPath
getLocationName = takeFileName

getFileSize :: FilePath -> IO FileOffset
getFileSize filepath = fmap fileSize (getFileStatus filepath)

getFileContents :: FilePath -> IO Char8.ByteString
getFileContents = Char8.readFile

excludeDotFolders :: [FilePath] -> [FilePath]
excludeDotFolders = filter (\filename -> head filename /= '.' )

-- ---------------------------------------------------------------

locationToAesonJSONVAlueRecursivelyComputedaAndStored :: LocationPath -> IO A.Value
locationToAesonJSONVAlueRecursivelyComputedaAndStored location = do
    isFile      <- Dir.doesFileExist location
    isDirectory <- Dir.doesDirectoryExist location
    if isFile
        then filepathToAesonJSONValue ( location :: FilePath )
        else if isDirectory
            then folderpathToAesonJSONValue ( location :: FolderPath )
            else return A.Null

--{
--	"aion-type" : "file"
--	"version"   : 1
--	"name"      : String
--	"size"      : Integer
--	"hash"      : sha1-hash
--}

filepathToAesonJSONValue :: FilePath -> IO A.Value
filepathToAesonJSONValue filepath = do
    filesize <- getFileSize filepath
    filecontents <- getFileContents filepath
    return $ GAOU.makeAesonValueForFileUsingFileContents ( getLocationName filepath ) ( fromIntegral filesize ) filecontents

--{
--	"aion-type" : "directory"
--	"version"   : 1
--	"name"      : String
--	"contents"  : [Aion-Hash]
--}

folderpathToAesonJSONValue :: FolderPath -> IO A.Value
folderpathToAesonJSONValue folderpath = do
    directoryContents <- Dir.getDirectoryContents folderpath
    aesonvalues <- mapM (\filename -> locationToAesonJSONVAlueRecursivelyComputedaAndStored $ folderpath </> filename)
                        (excludeDotFolders directoryContents)
    caskeys <- mapM GAOU.commitAesonValueToCAS aesonvalues
    let aesonvalues2 = map (A.String . T.pack) caskeys
    return $ GAOU.makeAesonValueForDirectoryUsingContentsAesonValues (getLocationName folderpath) aesonvalues2

-- ---------------------------------------------------------------

locationExists :: LocationPath -> IO Bool
locationExists locationpath = (||) <$> (Dir.doesDirectoryExist locationpath) <*> (Dir.doesFileExist locationpath)

computeMerkleRootForLocationRecursivelyComputedaAndStored :: LocationPath -> IO ( Maybe String )
computeMerkleRootForLocationRecursivelyComputedaAndStored locationpath = do
    exists <- locationExists locationpath
    if exists
        then do
            value <- locationToAesonJSONVAlueRecursivelyComputedaAndStored locationpath
            string <- GAOU.commitAesonValueToCAS value
            return $ Just string
        else
            return Nothing

commitMerkleRootForFSScanRoot :: String -> String -> IO ()
commitMerkleRootForFSScanRoot fsscanlocationpath merkleroot =
    X.set (FSRM.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan fsscanlocationpath) ( Char8.pack merkleroot )

getCurrentMerkleRootForFSScanRoot :: String -> IO (Maybe String)
-- xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan :: LocationPath -> String
-- get :: String -> IO ( Maybe Char8.ByteString )
getCurrentMerkleRootForFSScanRoot locationpath = do
    maybe_bytestring <- X.get (FSRM.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath)
    return $ fmap Char8.unpack ( maybe_bytestring )

-- ---------------------------------------------------------------

generalScan :: IO ()
generalScan = do
    scanroots <- FSRM.getFSScanRoots
    _ <- mapM (\scanroot -> do
                    s1 <- computeMerkleRootForLocationRecursivelyComputedaAndStored scanroot
                    case s1 of
                        Nothing -> return ()
                        Just s2 -> do
                            putStrLn $ "location: " ++ scanroot
                            putStrLn $ "merkle  : " ++ s2
                            commitMerkleRootForFSScanRoot scanroot s2
               ) scanroots
    return ()


