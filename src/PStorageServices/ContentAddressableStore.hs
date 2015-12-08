module PStorageServices.ContentAddressableStore (
    set,
    get
) where

import qualified PStorageServices.Xcache as X
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA       as SHA


makeKeyForValue :: Char8.ByteString -> String
makeKeyForValue bytestring = 
    let key1 = "24f4da84-5d85-4933-8775-dc6593dcad04:" ++ ( SHA.showDigest $ SHA.sha1 bytestring )
        key2 = SHA.showDigest $ SHA.sha1 $ Char8.pack key1 
    in key2

set :: Char8.ByteString -> IO String
set bytestring = do 
    let key = makeKeyForValue bytestring
    X.set key bytestring
    return key

get :: String -> IO ( Maybe Char8.ByteString )
get key = X.get key

