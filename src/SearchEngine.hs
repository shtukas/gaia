{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SearchEngine (
    getMerkleRoot,
    convertJSONStringIntoAeonJSONObject,
    extractLocationpathsForAionCASHashAndQuery,
    cycleQuery
) where

import qualified System.Directory as Dir

import qualified ContentAddressableStore

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Data.Maybe as M

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

type Locationpath = String

-- -----------------------------------------------------------

getMerkleRoot :: IO ( Maybe String )
getMerkleRoot = 
    do
        folderpath <- Dir.getAppUserDataDirectory "gaia"
        let filepath = folderpath Prelude.++ "/" Prelude.++"merkleroot"
        bool <- Dir.doesFileExist filepath
        if bool
            then do
                root <- readFile filepath
                return $ Just root
            else return Nothing

-- -----------------------------------------------------------

getAeonJSONStringForCASHash :: String -> IO String
getAeonJSONStringForCASHash hash = ContentAddressableStore.get hash

-- -----------------------------------------------------------

-- Prelude> import qualified Data.Aeson as A
-- Prelude A> import qualified Data.ByteString.Lazy.Char8 as Char8
-- Prelude A Char8> import Data.Maybe as D
-- Prelude A Char8 D> let value = A.decode (Char8.pack "{\"name\":\"Pascal\"}") :: Maybe A.Value
-- Prelude A Char8 D> value
-- Just (Object (fromList [("name",String "Pascal")]))
-- Prelude A Char8 D> D.fromJust value
-- Object (fromList [("name",String "Pascal")])

convertJSONStringIntoAeonJSONObject :: String -> Maybe A.Value
convertJSONStringIntoAeonJSONObject string = 
    let value1 = A.decode $ Char8.pack string
        value2 = if M.isJust value1
                    then 
                        value1
                    else 
                        Nothing
    in value2

-- -----------------------------------------------------------

-- extractLocationpathsForAionJsonFileObjectAndQuery <aeonObjectFile> <search pattern> <current path>

extractLocationpathsForAionJsonFileObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonFileObjectAndQuery aeonObjectFile pattern current_path = Just ["file"]

-- -----------------------------------------------------------

-- extractLocationpathsForAionJsonDirectoryObjectAndQuery <aeonObjectDirectory> <search pattern> <current path>

extractLocationpathsForAionJsonDirectoryObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonDirectoryObjectAndQuery aeonObjectDirectory pattern current_path = Just ["directory"]

-- -----------------------------------------------------------

-- extractLocationpathsForAionJsonObjectAndQuery <aeonObject> <search pattern> <current path>

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

    Structure of aeonObjects ( extracted using gaia-utils )

    Object (
        fromList [
            (
                "contents",Array (
                    fromList [
                        String "f2debbc0395676188af9224f21beebde4dfde586",
                        String "ac5b36985f766835d0e43d365d60ad3f242e0d04",
                        String "bbbb65c8fdbb5c24ae960ec832f8f3c72c6ed5f3",
                        String "74060dab7e3754a7e698b878a23540283b254971",
                        String "684ea29239107360c1a96e594810bc4235caf288"
                    ]
                )
            ),
            ("aion-type",String "directory"),
            ("name",String "Desktop"),
            ("version",Number 1.0)
        ]
    )

    Object (
        fromList [
            ("hash",String "49f83f1f31fb9605a6d22f81acd59a7b39a40e4b"),
            ("size",Number 2143190.0),("aion-type",String "file"),
            ("name",String "1449085780693.jpg"),
            ("version",Number 1.0)
        ]
    )

    an object is a HashMap
    an array is a Vector
    a string is a Text
    a number is Scientific 
        (
            because JSON doesn't specify precision and so a 
            type which allows arbitrary precision is used
        )

-}

extractListOfPairsFromAeonValue :: A.Value -> Maybe [(T.Text ,A.Value)]
extractListOfPairsFromAeonValue (A.Object x) = Just $ HM.toList x
extractListOfPairsFromAeonValue _ = Nothing

extractUnderlyingTextFromAeonString :: A.Value -> Maybe T.Text
extractUnderlyingTextFromAeonString (A.String x) = Just x
extractUnderlyingTextFromAeonString _ = Nothing

extractLocationpathsForAionJsonObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonObjectAndQuery aeonObject pattern current_path = 
    let 
        value1 = extractListOfPairsFromAeonValue aeonObject
        value2 = Prelude.lookup "aion-type" ( M.fromJust value1 )
        value3 = M.fromJust value2
        value4 = extractUnderlyingTextFromAeonString value3
        value5 = M.fromJust value4
    in
        if value5=="file"
            then
                extractLocationpathsForAionJsonFileObjectAndQuery aeonObject pattern current_path
            else
                extractLocationpathsForAionJsonDirectoryObjectAndQuery aeonObject pattern current_path

-- -----------------------------------------------------------

-- extractLocationpathsForAionCASHashAndQuery <aion cas hash> <search pattern> <current path>

extractLocationpathsForAionCASHashAndQuery :: String -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionCASHashAndQuery _ "" _ = do
    return $ Just []
extractLocationpathsForAionCASHashAndQuery aion_cas_hash pattern current_path = do
    aionJSONValueAsString <- ContentAddressableStore.get aion_cas_hash
    let aionJSONValueMaybe = convertJSONStringIntoAeonJSONObject aionJSONValueAsString
    if M.isJust aionJSONValueMaybe
        then do 
            let aionJSONValue = M.fromJust aionJSONValueMaybe
            return $ extractLocationpathsForAionJsonObjectAndQuery aionJSONValue pattern current_path 
        else
            return Nothing    

-- -----------------------------------------------------------

cycleQuery :: String -> IO ( Maybe [ Locationpath ] )
cycleQuery pattern = extractLocationpathsForAionCASHashAndQuery "79fbfe2749417f42d120d35229d6f4e61159f48a" pattern "/Users/pascal/Desktop"


