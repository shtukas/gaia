module Xcache (
    set,
    get,
    keyToFilename
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA       as SHA
import qualified Data.Time.Clock.POSIX      as Time
import           Gaia.FileSystem.Types
import qualified System.Directory           as Dir

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

getSha1Digest :: String -> String
getSha1Digest string = SHA.showDigest $ SHA.sha1 $ Char8.pack string

keyToFilename :: String -> String
keyToFilename key = "sha1-" ++ ( getSha1Digest key )

filenameToPathFragments :: String -> (String, String)
filenameToPathFragments filename = 
    let f1  = take 2 (drop 5 filename)
        f2  = take 2 (drop 7 filename)
    in  (f1, f2)

ensureFolderPath :: Folderpath -> IO ()
ensureFolderPath folderpath = Dir.createDirectoryIfMissing True folderpath

keyToDataFolderPath :: String -> Folderpath
keyToDataFolderPath key = 
    let 
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
        folderpath = "/x-space/xcache-v2/datablobs/"++fragment1++"/"++fragment2
    in  folderpath

keyToTimestampFolderPath :: String -> Folderpath
keyToTimestampFolderPath key = 
    let 
        filename = keyToFilename key
        (fragment1, fragment2) = filenameToPathFragments filename
        folderpath = "/x-space/xcache-v2/timestamps/"++fragment1++"/"++fragment2
    in  folderpath

keyToDataFilepath :: String -> IO Filepath
keyToDataFilepath key = 
    let folderpath = keyToDataFolderPath key
    in do
        ensureFolderPath folderpath
        return $ folderpath++"/"++(keyToFilename key)

keyToTimestampFilepath :: String -> IO Filepath
keyToTimestampFilepath key = 
    let folderpath = keyToTimestampFolderPath key
    in do
        ensureFolderPath folderpath
        return $ folderpath++"/"++(keyToFilename key)

set :: String -> String -> IO ()
set key value = do
    datafilepath <- keyToDataFilepath key
    timestampfilepath <- keyToTimestampFilepath key
    writeFile datafilepath value
    currenttime <- getCurrentUnixTime
    writeFile timestampfilepath $ show currenttime
    return ()

-- TODO: refactore to use `MaybeT IO String` and MonadPlus' guard
get :: String -> IO String
get key = do
    filepath <- keyToDataFilepath key
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            timestampfilepath <- keyToTimestampFilepath key
            currenttime <- getCurrentUnixTime
            writeFile timestampfilepath $ show currenttime
            readFile filepath
        else
            return ""
