module PStorageServices.Xcache (
    set, -- String -> String -> IO ()
    get  -- String -> IO ( Maybe String )
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA as SHA
import qualified Data.Maybe as M
import qualified Data.Time.Clock.POSIX as Time
import qualified Gaia.UserPreferences as UP
import qualified System.Directory as Dir
import qualified System.FilePath as FS

import           PStorageServices.Types

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

-- System.IO.Error
--     catchIOError :: IO a -> (IOError -> IO a) -> IO a

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

getSha1Digest :: Char8.ByteString -> String
getSha1Digest string = SHA.showDigest $ SHA.sha1 string

ensureFolderPath :: FolderPath -> IO ()
ensureFolderPath = Dir.createDirectoryIfMissing True

filenameToPathFragments :: String -> (String, String)
filenameToPathFragments filename =
    let f1  = take 2 (drop 5 filename)
        f2  = take 2 (drop 7 filename)
    in  (f1, f2)

keyToFilename :: String -> String
keyToFilename key = "sha1-" ++ getSha1Digest ( Char8.pack key )

keyToDataFolderPath :: String -> IO FolderPath
keyToDataFolderPath key =
    let
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
    in  UP.getXCacheRoot >>= \root ->
        return $ FS.normalise $ FS.joinPath [root, "datablobs", fragment1, fragment2]

keyToTimestampFolderPath :: String -> IO FolderPath
keyToTimestampFolderPath key =
    let
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
    in  UP.getXCacheRoot >>= \root ->
        return $ FS.normalise $ FS.joinPath [root, "timestamps", fragment1, fragment2]

keyToDataFilePath :: String -> IO FilePath
keyToDataFilePath key = do
    folderpath <- keyToDataFolderPath key
    ensureFolderPath folderpath
    return $ FS.normalise $ FS.joinPath [folderpath, keyToFilename key]

keyToTimestampFilePath :: String -> IO FilePath
keyToTimestampFilePath key = do
    folderpath <- keyToTimestampFolderPath key
    ensureFolderPath folderpath
    return $ FS.normalise $ FS.joinPath [folderpath, keyToFilename key]

set :: String -> Char8.ByteString -> IO ()
set key value = do
    datafilepath <- keyToDataFilePath key
    timestampfilepath <- keyToTimestampFilePath key
    Char8.writeFile datafilepath value
    currenttime <- getCurrentUnixTime
    writeFile timestampfilepath $ show currenttime
    return ()

get :: String -> IO ( Maybe Char8.ByteString )
get key = do
    filepath <- keyToDataFilePath key
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            timestampfilepath <- keyToTimestampFilePath key
            currenttime <- getCurrentUnixTime
            writeFile timestampfilepath $ show currenttime
            contents <- Char8.readFile filepath
            return $ M.Just contents
        else
            return Nothing
