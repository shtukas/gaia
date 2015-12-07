module ContentAddressableStore (
    set,
    get
) where

import qualified Xcache
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Digest.Pure.SHA       as SHA

set :: Char8.ByteString -> IO String
set value = 
    let key1 = "24f4da84-5d85-4933-8775-dc6593dcad04:" ++ ( SHA.showDigest $ SHA.sha1 value )
        key2 = SHA.showDigest $ SHA.sha1 $ Char8.pack key1 
    in do 
        Xcache.set ( key2 ) value
        return key2

get :: String -> IO ( Maybe Char8.ByteString )
get key = Xcache.get key

