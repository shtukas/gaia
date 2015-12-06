module Xcache (
    set, -- String -> String -> IO ()
    get  -- String -> IO ( Maybe String )
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA       as SHA
import qualified Data.Time.Clock.POSIX      as Time
import qualified System.Directory           as Dir
import qualified Data.Maybe as M
import qualified UserPreferences

type Filepath = String
type Folderpath = String

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

-- System.IO.Error
--     catchIOError :: IO a -> (IOError -> IO a) -> IO a

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

getSha1Digest :: String -> String
getSha1Digest string = SHA.showDigest $ SHA.sha1 $ Char8.pack string

ensureFolderPath :: Folderpath -> IO ()
ensureFolderPath = Dir.createDirectoryIfMissing True

filenameToPathFragments :: String -> (String, String)
filenameToPathFragments filename =
    let f1  = take 2 (drop 5 filename)
        f2  = take 2 (drop 7 filename)
    in  (f1, f2)

keyToFilename :: String -> String
keyToFilename key = "sha1-" ++ getSha1Digest key

keyToDataFolderPath :: String -> IO Folderpath
keyToDataFolderPath key =
    let
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
    in  UserPreferences.getXCacheRoot >>= \root ->
        return $ root ++ "/datablobs/" ++ fragment1 ++ "/" ++ fragment2

keyToTimestampFolderPath :: String -> IO Folderpath
keyToTimestampFolderPath key =
    let
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
    in  UserPreferences.getXCacheRoot >>= \root ->
        return $ root ++ "/timestamps/" ++ fragment1 ++ "/" ++ fragment2

keyToDataFilePath :: String -> IO Filepath
keyToDataFilePath key = do
    folderpath <- keyToDataFolderPath key
    ensureFolderPath folderpath
    return $ folderpath ++ "/" ++ keyToFilename key

keyToTimestampFilePath :: String -> IO Filepath
keyToTimestampFilePath key = do
    folderpath <- keyToTimestampFolderPath key
    ensureFolderPath folderpath
    return $ folderpath ++ "/" ++ keyToFilename key

set :: String -> String -> IO ()
set key value = do
    datafilepath <- keyToDataFilePath key
    timestampfilepath <- keyToTimestampFilePath key
    writeFile datafilepath value
    currenttime <- getCurrentUnixTime
    writeFile timestampfilepath $ show currenttime
    return ()

-- TODO: refactore to use `MaybeT IO String` and MonadPlus' guard
get :: String -> IO ( Maybe String )
get key = do
    filepath <- keyToDataFilePath key
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            timestampfilepath <- keyToTimestampFilePath key
            currenttime <- getCurrentUnixTime
            writeFile timestampfilepath $ show currenttime
            contents <- readFile filepath
            return $ M.Just contents
        else
            return Nothing
