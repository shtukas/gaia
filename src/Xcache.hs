module Xcache (
    set,
    get
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Time.Clock.POSIX      as Time
import qualified System.Directory           as Dir
import           Data.Digest.Pure.SHA       as SHA

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

getDigestAndFragments :: String -> (String, String, String)
getDigestAndFragments key = let
        digest = SHA.showDigest $ SHA.sha1 $ Char8.pack key
        fragment1 = take 2 digest
        fragment2 = take 2 fragment1
    in (digest, fragment1, fragment2)

ensureAndGetFolderPathWithPrefix :: String -> String -> String -> IO String
ensureAndGetFolderPathWithPrefix p f f' =
    let folderpath = p ++ "/" ++ f ++ "/" ++ f' in
    Dir.createDirectoryIfMissing True folderpath
    >> return folderpath

keyToDataFilepathEnsureParentFolder :: String -> IO String
keyToDataFilepathEnsureParentFolder key = let
        (digest, fragment1, fragment2) = getDigestAndFragments key
        filename = "sha1-" ++ digest
    in do
        folderpath <- ensureAndGetFolderPathWithPrefix
                        "/x-space/xcache-v2/datablobs" fragment1 fragment2
        let filepath = folderpath ++ "/" ++ filename
        return filepath

keyToTimestampFilepathEnsureParentFolder :: String -> IO String
keyToTimestampFilepathEnsureParentFolder key = let
        (digest, fragment1, fragment2) = getDigestAndFragments key
        filename = "sha1-" ++ digest
    in do
        folderpath <- ensureAndGetFolderPathWithPrefix
                        "/x-space/xcache-v2/timestamps" fragment1 fragment2
        let filepath = folderpath ++ "/" ++ filename
        return filepath

set :: String -> String -> IO ()
set key value = do
    datafilepath <- keyToDataFilepathEnsureParentFolder key
    timestampfilepath <- keyToTimestampFilepathEnsureParentFolder key
    writeFile datafilepath value
    currenttime <- getCurrentUnixTime
    writeFile timestampfilepath $ show currenttime
    return ()

get :: String -> IO String
get key = do
    filepath <- keyToDataFilepathEnsureParentFolder key
    fileExists <- Dir.doesFileExist filepath
    if fileExists
        then do
            timestampfilepath <- keyToTimestampFilepathEnsureParentFolder key
            currenttime <- getCurrentUnixTime
            writeFile timestampfilepath $ show currenttime
            readFile filepath
        else
            return ""
