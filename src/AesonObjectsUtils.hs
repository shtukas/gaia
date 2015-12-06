{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AesonObjectsUtils where

-- This module concentrates utility functions to facilitate the reading of Aeson Objects

import qualified Data.Aeson                 as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Data.Maybe                 as M

import qualified Data.Text                  as T

import qualified Data.HashMap.Strict        as HM

import qualified GHC.Exts                   as E
    -- support for the JSON library

import           Data.Scientific

import           Data.Digest.Pure.SHA       as SHA
    -- SHA.sha1 :: Char8.ByteString -> Digest SHA1State
    -- SHA.showDigest :: Digest t -> String

import           ContentAddressableStore

import qualified Data.Vector                as V

import           Data.Scientific            as S

import qualified ContentAddressableStore

type Locationpath = String

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

    Structure of aesonObjects ( extracted using gaia-utils )

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

-- -----------------------------------------------------------
-- Getting JSON Strings From Storage
-- -----------------------------------------------------------

getAesonJSONStringForCASHash :: String -> IO String
getAesonJSONStringForCASHash hash = ContentAddressableStore.get hash

-- -----------------------------------------------------------
-- Building Aeson Values
-- -----------------------------------------------------------

makeAesonJSONValueForFile :: String -> Integer -> Char8.ByteString -> A.Value
makeAesonJSONValueForFile filename filesize filecontents =
    A.Object $ E.fromList [
        ("aion-type" , A.String "file"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack filename),
        ("size"      , A.Number $ scientific filesize 1 ),
        ("hash"      , A.String $ T.pack $ SHA.showDigest $ SHA.sha1 filecontents) ]

makeAesonJSONValueForDirectory :: String -> [A.Value] -> A.Value
makeAesonJSONValueForDirectory foldername aesonvalues =
    A.Object $ E.fromList [
            ("aion-type" , A.String "directory"),
            ("version"   , A.Number 1),
            ("name"      , A.String $ T.pack foldername),
            ("contents"  , A.Array $ V.fromList aesonvalues ) ]

-- -----------------------------------------------------------
-- Aeson Values to JSON String (and Storage)
-- -----------------------------------------------------------

aesonJSONVAlueToString :: A.Value -> String
aesonJSONVAlueToString value = Char8.unpack $ A.encode value

commitAesonJSONValueToCAS :: A.Value -> IO String
commitAesonJSONValueToCAS value = ContentAddressableStore.set $ aesonJSONVAlueToString value

-- -----------------------------------------------------------
--  JSON Strings to Aeson Values
-- -----------------------------------------------------------

-- Prelude> import qualified Data.Aeson as A
-- Prelude A> import qualified Data.ByteString.Lazy.Char8 as Char8
-- Prelude A Char8> import Data.Maybe as D
-- Prelude A Char8 D> let value = A.decode (Char8.pack "{\"name\":\"Pascal\"}") :: Maybe A.Value
-- Prelude A Char8 D> value
-- Just (Object (fromList [("name",String "Pascal")]))
-- Prelude A Char8 D> D.fromJust value
-- Object (fromList [("name",String "Pascal")])

convertJSONStringIntoAesonJSONObject :: String -> Maybe A.Value
convertJSONStringIntoAesonJSONObject string =
    let value1 = A.decode $ Char8.pack string
        value2 = if M.isJust value1
            then 
                value1
            else 
                Nothing
    in value2

-- -----------------------------------------------------------
-- Extracting Data from Aeson Value
-- -----------------------------------------------------------

extractListOfPairsFromAesonValueObject :: A.Value -> Maybe [(T.Text ,A.Value)]
extractListOfPairsFromAesonValueObject (A.Object x) = Just $ HM.toList x
extractListOfPairsFromAesonValueObject _ = Nothing

extractUnderlyingTextFromAesonValueString :: A.Value -> Maybe T.Text
extractUnderlyingTextFromAesonValueString (A.String x) = Just x
extractUnderlyingTextFromAesonValueString _ = Nothing

extractUnderlyingIntegerFromAesonValueNumber :: A.Value -> Maybe Integer
extractUnderlyingIntegerFromAesonValueNumber (A.Number x) = Just $ S.coefficient x
extractUnderlyingIntegerFromAesonValueNumber _ = Nothing

extractUnderlyingListOfStringsFromAesonValueVectorString :: A.Value -> Maybe [String]
extractUnderlyingListOfStringsFromAesonValueVectorString (A.Array x) = Just $ map (\v -> (T.unpack . M.fromJust . extractUnderlyingTextFromAesonValueString) v ) ( V.toList x )
extractUnderlyingListOfStringsFromAesonValueVectorString _ = Nothing

aesonValueForFileGaiaProjection :: A.Value -> ( String, Integer, String ) -- ( filename, filesize, sha1-shah )
aesonValueForFileGaiaProjection aValue =
    let
        value1 = extractListOfPairsFromAesonValueObject aValue                     -- [(T.Text ,A.Value)]

        value2 = M.fromJust $ Prelude.lookup "name" ( M.fromJust value1 )         -- AesonValueString
        value3 = M.fromJust $ extractUnderlyingTextFromAesonValueString value2     -- Text
        filename = T.unpack value3                                                -- String

        value4 = M.fromJust $ Prelude.lookup "size" ( M.fromJust value1 )         -- AesonValueNumber  
        value5 = M.fromJust $ extractUnderlyingIntegerFromAesonValueNumber value4  -- Integer
        filesize = value5

        value6 = M.fromJust $ Prelude.lookup "hash" ( M.fromJust value1 )         -- AesonValueNumber  
        value7 = M.fromJust $ extractUnderlyingTextFromAesonValueString value6     -- Integer
        hash = T.unpack value7

    in (filename,filesize,hash)

aesonValueForDirectoryGaiaProjection :: A.Value -> ( String, [String] ) -- ( foldername, [CAS-Keys(s)] ) ( String, [String] )
aesonValueForDirectoryGaiaProjection aValue =
    let
        value1 = extractListOfPairsFromAesonValueObject aValue
        value2 = M.fromJust $ Prelude.lookup "name" ( M.fromJust value1 )
        value3 = M.fromJust $ extractUnderlyingTextFromAesonValueString value2
        foldername = T.unpack value3

        value6 = M.fromJust $ Prelude.lookup "contents" ( M.fromJust value1 )                 -- AesonValueNumber  
        value7 = M.fromJust $ extractUnderlyingListOfStringsFromAesonValueVectorString value6  -- [String]
        contents = value7

    in (foldername, contents)
