module Xcache (
    set,
    get
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Time.Clock.POSIX      as Time
import qualified System.Directory           as Dir
import           Data.Digest.Pure.SHA       as SHA
import Gaia.FileSystem.Types

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

getDigestAndFragments :: String -> (DigestString, DigestFragment, DigestFragment)
getDigestAndFragments k = let
        d  = SHA.showDigest $ SHA.sha1 $ Char8.pack k
        f  = take 2 d
        f' = take 2 f
    in (d, f, f')

ensureAndGetFolderPathWithPrefix :: Folderpath -> Folderpath -> Folderpath -> IO Folderpath
ensureAndGetFolderPathWithPrefix p f f' =
    let folderpath = p ++ "/" ++ f ++ "/" ++ f' in
    Dir.createDirectoryIfMissing True folderpath
    >> return folderpath

keyToDataFilepathEnsureParentFolder :: String -> IO Filepath
keyToDataFilepathEnsureParentFolder key = let
        (digest, frag, frag') = getDigestAndFragments key
        filename = "sha1-" ++ digest
    in do
        folderpath <- ensureAndGetFolderPathWithPrefix
                        "/x-space/xcache-v2/datablobs" frag frag'
        let filepath = folderpath ++ "/" ++ filename 
        return filepath

keyToTimestampFilepathEnsureParentFolder :: String -> IO Filepath
keyToTimestampFilepathEnsureParentFolder key = let
        (digest, frag, frag') = getDigestAndFragments key
        filename = "sha1-" ++ digest
    in do
        folderpath <- ensureAndGetFolderPathWithPrefix
                        "/x-space/xcache-v2/timestamps" frag frag'
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

-- TODO: refactore to use `MaybeT IO String` and MonadPlus' guard
get :: String -> IO String
get key = do
    filepath <- keyToDataFilepathEnsureParentFolder key
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            timestampfilepath <- keyToTimestampFilepathEnsureParentFolder key
            currenttime <- getCurrentUnixTime
            writeFile timestampfilepath $ show currenttime
            readFile filepath
        else
            return ""
