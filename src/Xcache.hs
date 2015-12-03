module Xcache (
    set,
    get
) where

import Data.Digest.Pure.SHA as SHA
import Data.ByteString.Lazy.Char8 as Char8
import qualified System.Directory as Dir
import qualified Data.Time.Clock.POSIX as Time

-- Data.Digest.Pure.SHA
--     sha1 :: Data.ByteString.Lazy.Internal.ByteString -> Digest SHA1State
--     showDigest :: Digest t -> String

-- Data.ByteString.Lazy.Char8
--     pack :: [Char] -> ByteString

getCurrentUnixTime :: IO Int
getCurrentUnixTime = round `fmap` Time.getPOSIXTime

keyToDataFilepathEnsureParentFolder :: String -> IO String
keyToDataFilepathEnsureParentFolder key = do
    let filename = "sha1-" ++ ( SHA.showDigest $ SHA.sha1 $ Char8.pack key )
    let fragment1 = Prelude.take 2 ( Prelude.drop 5 filename )
    let fragment2 = Prelude.take 2 ( Prelude.drop 7 filename )
    let folderpath = "/x-space/xcache-v2/datablobs/"++fragment1++"/"++fragment2
    let filepath = folderpath++"/"++filename
    Dir.createDirectoryIfMissing True folderpath
    return filepath

keyToTimestampFilepathEnsureParentFolder :: String -> IO String
keyToTimestampFilepathEnsureParentFolder key = do
    let filename = "sha1-" ++ ( SHA.showDigest $ SHA.sha1 $ Char8.pack key )
    let fragment1 = Prelude.take 2 ( Prelude.drop 5 filename )
    let fragment2 = Prelude.take 2 ( Prelude.drop 7 filename )
    let folderpath = "/x-space/xcache-v2/timestamps/"++fragment1++"/"++fragment2
    let filepath = folderpath++"/"++filename
    Dir.createDirectoryIfMissing True folderpath
    return filepath

set :: String -> String -> IO ()
set key value = do
    datafilepath <- keyToDataFilepathEnsureParentFolder key
    timestampfilepath <- keyToTimestampFilepathEnsureParentFolder key
    Prelude.writeFile datafilepath value
    currenttime <- getCurrentUnixTime
    Prelude.writeFile timestampfilepath $ show currenttime
    return ()

get :: String -> IO String
get key = do 
    filepath <- keyToDataFilepathEnsureParentFolder key 
    fileExists <- Dir.doesFileExist filepath
    if fileExists
        then do
            timestampfilepath <- keyToTimestampFilepathEnsureParentFolder key
            currenttime <- getCurrentUnixTime
            Prelude.writeFile timestampfilepath $ show currenttime
            contents <- Prelude.readFile filepath  
            return contents 
        else do
            return ""




