{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AeonObjectsUtils where

-- This module concentrates utility functions to facilitate the reading of Aeon Objects

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

-- -----------------------------------------------------------
-- Building Aeon Values
-- -----------------------------------------------------------

makeAeonJSONValueForFile :: String -> Integer -> Char8.ByteString -> A.Value
makeAeonJSONValueForFile filename filesize filecontents =
    A.Object $ E.fromList [
        ("aion-type" , A.String "file"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack filename),
        ("size"      , A.Number $ scientific filesize 1 ),
        ("hash"      , A.String $ T.pack $ SHA.showDigest $ SHA.sha1 filecontents) ]

makeAeonJSONValueForDirectory :: String -> [A.Value] -> A.Value
makeAeonJSONValueForDirectory foldername aeonvalues =
    A.Object $ E.fromList [
            ("aion-type" , A.String "directory"),
            ("version"   , A.Number 1),
            ("name"      , A.String $ T.pack foldername),
            ("contents"  , A.Array $ V.fromList aeonvalues ) ]

-- -----------------------------------------------------------
-- Aeon Values to JSON String (and Storage)
-- -----------------------------------------------------------

aeonJSONVAlueToString :: A.Value -> String
aeonJSONVAlueToString value = Char8.unpack $ A.encode value

commitAeonJSONValueToCAS :: A.Value -> IO String
commitAeonJSONValueToCAS value = ContentAddressableStore.set $ aeonJSONVAlueToString value

-- -----------------------------------------------------------
--  JSON Strings to Aeon Values
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
-- Extracting Data from Aeon Value
-- -----------------------------------------------------------

extractListOfPairsFromAeonValue :: A.Value -> Maybe [(T.Text ,A.Value)]
extractListOfPairsFromAeonValue (A.Object x) = Just $ HM.toList x
extractListOfPairsFromAeonValue _ = Nothing

extractUnderlyingTextFromAeonString :: A.Value -> Maybe T.Text
extractUnderlyingTextFromAeonString (A.String x) = Just x
extractUnderlyingTextFromAeonString _ = Nothing






