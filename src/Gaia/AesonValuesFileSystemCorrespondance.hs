{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.AesonValuesFileSystemCorrespondance (
    getAionJSONStringForCASKey3,
    convertJSONStringIntoAesonValue,
    aesonVAlueToString,
    commitStringToCAS
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified PStorageServices.ContentAddressableStore as CAS

{-|

    Aion Objects

    {
        "aion-type" : "file"
        "version"   : 1
        "name"      : String
        "size"      : Integer
        "hash"      : sha1-hash
    }
    {
        "aion-type" : "directory"
        "version"   : 1
        "name"      : String
        "contents"  : CAS-KEY(s)
    }

-}

{-|

    Aeson Values

    Object (
        fromList [
            ("aion-type",   String "directory")
            ("version",     Number 1.0)
            ("name",        String "Desktop")
            (
                "contents", Array (
                    fromList [
                        String "f2debbc0395676188af9224f21beebde4dfde586",
                        String "ac5b36985f766835d0e43d365d60ad3f242e0d04",
                        String "bbbb65c8fdbb5c24ae960ec832f8f3c72c6ed5f3",
                        String "74060dab7e3754a7e698b878a23540283b254971",
                        String "684ea29239107360c1a96e594810bc4235caf288"
                    ]
                )
            )
        ]
    )

    Object (
        fromList [
            ("aion-type", String "file")
            ("version",   Number 1.0)
            ("name",      String "1449085780693.jpg")
            ("size",      Number 2143190.0)
            ("hash",      String "49f83f1f31fb9605a6d22f81acd59a7b39a40e4b")
        ]
    )

    Object is a HashMap
    Array  is a Vector
    String is a Text
    Number is a Scientific
        (
            because JSON doesn't specify precision and so a
            type which allows arbitrary precision is used
        )

-}

getAionJSONStringForCASKey3 :: String -> IO (Maybe String)
getAionJSONStringForCASKey3 hash = do 
    string <- CAS.get hash
    return $ fmap Char8.unpack (string)

convertJSONStringIntoAesonValue :: String -> Maybe A.Value
convertJSONStringIntoAesonValue string = A.decode $ Char8.pack string

aesonVAlueToString :: A.Value -> String
aesonVAlueToString value = Char8.unpack $ A.encode value

commitStringToCAS :: String -> IO String
commitStringToCAS string = CAS.set $ Char8.pack string

