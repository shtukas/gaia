{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.ScanningAndRecordingManager (
    getCurrentMerkleRootForFSScanRoot,
    generalScan
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Digest.Pure.SHA as SHA
import           Data.Maybe
import qualified Gaia.AionPointAbstractionUtils as GAOU
import qualified Gaia.FSRootsManagement as FSRM
import           Gaia.Types
import qualified PStorageServices.Xcache as X
import qualified System.Directory as Dir
import           System.FilePath
import           System.Posix               (FileOffset, fileSize,
                                             getFileStatus)

-- ---------------------------------------------------------------
-- Utils 
-- ---------------------------------------------------------------

locationExists :: LocationPath -> IO Bool
locationExists locationpath = (||) <$> (Dir.doesDirectoryExist locationpath) <*> (Dir.doesFileExist locationpath)

commitMerkleRootForFSScanRoot :: String -> String -> IO ()
commitMerkleRootForFSScanRoot fsscanlocationpath merkleroot =
    X.set (FSRM.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan fsscanlocationpath) ( Char8.pack merkleroot )

getCurrentMerkleRootForFSScanRoot :: String -> IO (Maybe String)
getCurrentMerkleRootForFSScanRoot locationpath = do
    maybe_bytestring <- X.get (FSRM.xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath)
    return $ fmap Char8.unpack ( maybe_bytestring )

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

filepathToAionPointAbstractionFile :: FilePath -> IO (Maybe AionPointAbstractionFile)
filepathToAionPointAbstractionFile filepath = do
    isFile <- Dir.doesFileExist filepath
    if isFile
        then do
            filesize <- getFileSize filepath
            filecontents <- getFileContents filepath
            return $ Just $ AionPointAbstractionFile ( getLocationName (filepath :: LocationPath) ) ( fromIntegral filesize ) (SHA.showDigest $ SHA.sha1 filecontents)
        else
            return Nothing

folderpathToAionPointAbstractionDirectory :: FolderPath -> IO (Maybe AionPointAbstractionDirectory)
folderpathToAionPointAbstractionDirectory folderpath = do
    isDirectory <- Dir.doesDirectoryExist folderpath
    if isDirectory
        then do
            directoryContents <- Dir.getDirectoryContents folderpath
            let x1 = map (\filename -> locationToAionPointAbstractionGenericRecursivelyComputedaAndStored $ folderpath </> filename)
                  (excludeDotFolders directoryContents)
            -- x1 :: [IO (Maybe AionPointAbstractionGeneric)]
            let x2 = sequence x1
            -- x2 :: IO [Maybe AionPointAbstractionGeneric]
            -- catMaybes :: [Maybe a] -> [a]
            let x3 = fmap catMaybes x2
            -- x3 :: IO [AionPointAbstractionGeneric]
            x4 <- x3
            -- x4 :: [AionPointAbstractionGeneric]
            caskeys <- sequence $ map GAOU.commitAionPointAbstractionGenericToCAS x4
            return $ Just $ AionPointAbstractionDirectory (getLocationName folderpath) caskeys
        else
            return Nothing

locationToAionPointAbstractionGenericRecursivelyComputedaAndStored :: LocationPath -> IO (Maybe AionPointAbstractionGeneric)
locationToAionPointAbstractionGenericRecursivelyComputedaAndStored location = do
    isFile      <- Dir.doesFileExist location       -- The operation doesFileExist returns True if the argument file exists and is not a directory, and False otherwise.
    isDirectory <- Dir.doesDirectoryExist location  -- The operation doesDirectoryExist returns True if the argument file exists and is a directory, and False otherwise.
    if (not isFile) && (not isDirectory) 
        then
            return Nothing
        else
            if isFile
                then do 
                    x1 <- filepathToAionPointAbstractionFile (location :: FilePath)
                    case x1 of
                        Nothing -> return Nothing
                        Just x2 -> return $ Just (AionPointAbstractionGenericFromFile x2)
                else do
                     x1 <- folderpathToAionPointAbstractionDirectory (location :: FolderPath)
                     case x1 of
                        Nothing -> return Nothing
                        Just x2 -> return $ Just (AionPointAbstractionGenericFromDirectory x2)

-- ---------------------------------------------------------------

computeMerkleRootForLocationRecursivelyComputedaAndStored :: LocationPath -> IO (Maybe String)
computeMerkleRootForLocationRecursivelyComputedaAndStored locationpath = do
    exists <- locationExists locationpath
    if exists
        then do
            value <- locationToAionPointAbstractionGenericRecursivelyComputedaAndStored locationpath
            case value of
                Nothing     -> return Nothing
                Just value' -> do 
                    x1 <- GAOU.commitAionPointAbstractionGenericToCAS value'
                    return $ Just x1
        else
            return Nothing

-- ---------------------------------------------------------------

generalScan :: IO ()
generalScan = do
    scanroots <- FSRM.getFSScanRoots
    _ <- sequence $ fmap (\scanroot -> do
                        s1 <- computeMerkleRootForLocationRecursivelyComputedaAndStored scanroot
                        case s1 of
                            Nothing -> return ()
                            Just s2 -> do
                                putStrLn $ "location: " ++ scanroot
                                putStrLn $ "merkle  : " ++ s2
                                commitMerkleRootForFSScanRoot scanroot s2
                   ) scanroots
    return ()

