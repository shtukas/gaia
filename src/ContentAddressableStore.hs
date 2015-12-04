module ContentAddressableStore (
    set,
    get
) where

import qualified Xcache
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA       as SHA

set :: String -> IO String
set value = 
    let key = Xcache.keyToFilename $ SHA.showDigest $ SHA.sha1 $ Char8.pack value
    in do 
        Xcache.set ( key ) value
        return $ Xcache.keyToFilename key

get :: String -> IO String
get key = Xcache.get key

