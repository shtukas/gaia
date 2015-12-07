{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.AesonObjectsUtils where

-- This module concentrates utility functions to facilitate the reading of Aeson Objects

import           Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Digest.Pure.SHA as SHA
    -- SHA.sha1 :: Char8.ByteString -> Digest SHA1State
    -- SHA.showDigest :: Digest t -> String
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GHC.Exts as E
    -- support for the JSON library
import qualified PStorageServices.ContentAddressableStore as CAS

import           Gaia.Types

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

getAesonJSONStringForCASKey :: String -> MaybeT IO String
getAesonJSONStringForCASKey hash = do
    string <- MaybeT $ CAS.get hash
    return $ Char8.unpack string

-- -----------------------------------------------------------
-- Building Aeson Values
-- -----------------------------------------------------------

makeAesonValueForFile :: String -> Integer -> Char8.ByteString -> A.Value
makeAesonValueForFile filename filesize filecontents =
    A.Object $ E.fromList [
        ("aion-type" , A.String "file"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack filename),
        ("size"      , A.Number $ S.scientific filesize 1 ),
        ("hash"      , A.String $ T.pack $ SHA.showDigest $ SHA.sha1 filecontents) ]

makeAesonValueForDirectory :: String -> [A.Value] -> A.Value
makeAesonValueForDirectory foldername aesonvalues =
    A.Object $ E.fromList [
            ("aion-type" , A.String "directory"),
            ("version"   , A.Number 1),
            ("name"      , A.String $ T.pack foldername),
            ("contents"  , A.Array $ V.fromList aesonvalues ) ]

-- -----------------------------------------------------------
-- Aeson Values to JSON String (and Storage)
-- -----------------------------------------------------------

aesonVAlueToString :: A.Value -> String
aesonVAlueToString value = Char8.unpack $ A.encode value

commitAesonValueToCAS :: A.Value -> IO String
commitAesonValueToCAS value = CAS.set $ Char8.pack $ aesonVAlueToString value

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

convertJSONStringIntoAesonValue :: String -> Maybe A.Value
convertJSONStringIntoAesonValue string = A.decode $ Char8.pack string

-- -----------------------------------------------------------
-- Extracting Data from Aeson Value
-- -----------------------------------------------------------

aesonValueIsFile :: A.Value -> Bool
aesonValueIsFile aesonValue =
    let 
        value1 = extractListOfPairsFromAesonValue aesonValue []
        answer = case lookup "aion-type" value1 of 
            Nothing     -> False
            Just value3 -> ( extractUnderlyingTextFromAesonValueString value3 "" )=="file"
    in
        answer

extractListOfPairsFromAesonValue :: A.Value -> [(T.Text ,A.Value)] -> [(T.Text ,A.Value)]
extractListOfPairsFromAesonValue (A.Object x) _ = HM.toList x
extractListOfPairsFromAesonValue _ defaultvalue = defaultvalue

extractUnderlyingTextFromAesonValueString :: A.Value -> String -> T.Text
extractUnderlyingTextFromAesonValueString (A.String x) _ = x
extractUnderlyingTextFromAesonValueString _ defaultvalue = T.pack defaultvalue

extractUnderlyingIntegerFromAesonValueNumber :: A.Value -> Integer -> Integer
extractUnderlyingIntegerFromAesonValueNumber (A.Number x) _ = S.coefficient x
extractUnderlyingIntegerFromAesonValueNumber _ defaultvalue = defaultvalue 

extractUnderlyingListOfStringsFromAesonValueVectorString :: A.Value -> [String] -> [String]
extractUnderlyingListOfStringsFromAesonValueVectorString (A.Array x) _ = map (\v -> (T.unpack $ extractUnderlyingTextFromAesonValueString v "" ) ) ( V.toList x )
extractUnderlyingListOfStringsFromAesonValueVectorString _ defaultvalue = defaultvalue

aesonValueForFileGaiaProjection :: A.Value -> ( String, Integer, String ) -- ( filename, filesize, sha1-shah )
aesonValueForFileGaiaProjection aValue =
    let
        value1 = extractListOfPairsFromAesonValue aValue [] -- [(T.Text ,A.Value)]

        filename = 
            case lookup "name" value1 of
                Nothing -> "" 
                Just v2 -> T.unpack $ extractUnderlyingTextFromAesonValueString v2 ""

        filesize =
            case lookup "size" value1 of
                Nothing -> 0
                Just s1 -> extractUnderlyingIntegerFromAesonValueNumber s1 0

        hash = 
            case lookup "hash" value1 of 
                Nothing -> ""
                Just h1 -> T.unpack $ extractUnderlyingTextFromAesonValueString h1 ""

    in (filename,filesize,hash)

aesonValueForDirectoryGaiaProjection :: A.Value -> ( String, [String] ) -- ( foldername, [CAS-Keys(s)] ) ( String, [String] )
aesonValueForDirectoryGaiaProjection aValue =
    let
        value1 = extractListOfPairsFromAesonValue aValue []

        foldername = 
            case lookup "name" value1 of
                Nothing -> "" 
                Just v2 -> T.unpack $ extractUnderlyingTextFromAesonValueString v2 "" 

        contents = 
            case lookup "contents" value1 of
                Nothing -> []
                Just c1 -> extractUnderlyingListOfStringsFromAesonValueVectorString c1 []

    in (foldername, contents)

-- -----------------------------------------------------------------------------
-- Because manipulating Aeson values in a little bit painful we are going to use 
-- the datatypes we defined

aesonValueToTAionPoint :: A.Value -> TAionPoint
aesonValueToTAionPoint aesonvalue  
    | aesonValueIsFile aesonvalue = 
        let 
            value1 = extractListOfPairsFromAesonValue aesonvalue [] -- [(T.Text ,A.Value)]

            filename = 
                case Prelude.lookup "name" value1 of
                    Nothing -> "" 
                    Just v2 -> T.unpack $ extractUnderlyingTextFromAesonValueString v2 ""

            filesize =
                case Prelude.lookup "size" value1 of
                    Nothing -> 0
                    Just s1 -> extractUnderlyingIntegerFromAesonValueNumber s1 0

            hash = 
                case Prelude.lookup "hash" value1 of 
                    Nothing -> ""
                    Just h1 -> T.unpack $ extractUnderlyingTextFromAesonValueString h1 ""            

        in TAionPointFile filename filesize hash
    | otherwise = 
        -- Here we make a leap of faith that if it's not a file it's a directory
        -- TODO: understand if is worth to move it to Either or Maybe with a 
        --       check isDirectory or simply brutally panic if not a directory :P
        let
            value1 = extractListOfPairsFromAesonValue aesonvalue []

            foldername = 
                case Prelude.lookup "name" value1 of
                    Nothing -> "" 
                    Just v2 -> T.unpack $ extractUnderlyingTextFromAesonValueString v2 "" 

            contents = 
                case Prelude.lookup "contents" value1 of
                    Nothing -> []
                    Just c1 -> extractUnderlyingListOfStringsFromAesonValueVectorString c1 []            
        in TAionPointDirectory foldername contents

