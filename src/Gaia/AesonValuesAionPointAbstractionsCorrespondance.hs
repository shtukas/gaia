{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.AesonValuesAionPointAbstractionsCorrespondance (
    extendedAesonValueToExtendedAionPointAbstractionGeneric,
    tAionPointToAesonValue
) where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import           Gaia.Types
import qualified GHC.Exts as E -- support for the JSON library (Aeson)

{-

    Aeson Values ( extracted using gaia-utils )

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
extractUnderlyingListOfStringsFromAesonValueVectorString (A.Array x) _ =
    map (\v -> T.unpack $ extractUnderlyingTextFromAesonValueString v "" )
        ( V.toList x )
extractUnderlyingListOfStringsFromAesonValueVectorString _ defaultvalue = defaultvalue

aesonValueIsFile :: A.Value -> Bool
aesonValueIsFile aesonValue =
    let v = extractListOfPairsFromAesonValue aesonValue [] in
    case lookup "aion-type" v of
        Nothing     -> False
        Just v' -> ( extractUnderlyingTextFromAesonValueString v' "" )=="file"
    
extendedAesonValueToExtendedAionPointAbstractionGeneric :: ExtendedAesonValue -> ExtendedAionPointAbstractionGeneric
extendedAesonValueToExtendedAionPointAbstractionGeneric (ExtendedAesonValue aesonvalue caskey)
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
        in ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromFile (AionPointAbstractionFile filename filesize hash)) caskey
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
        in ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory foldername contents)) caskey

makeAesonValueForFileUsingKnownFileHash :: String -> Integer -> String -> A.Value
makeAesonValueForFileUsingKnownFileHash filename filesize hash =
    A.Object $ E.fromList [
        ("aion-type" , A.String "file"),
        ("version"   , A.Number 1),
        ("name"      , A.String $ T.pack filename),
        ("size"      , A.Number $ S.scientific filesize 1 ),
        ("hash"      , A.String $ T.pack hash) ]

makeAesonValueForDirectoryUsingContentsHashes :: String -> [String] -> A.Value
makeAesonValueForDirectoryUsingContentsHashes foldername hashes =
    A.Object $ E.fromList [
            ("aion-type" , A.String "directory"),
            ("version"   , A.Number 1),
            ("name"      , A.String $ T.pack foldername),
            ("contents"  , A.Array $ V.fromList ( map (A.String . T.pack ) hashes ) ) ]

tAionPointToAesonValue :: AionPointAbstractionGeneric -> A.Value
tAionPointToAesonValue (AionPointAbstractionGenericFromFile (AionPointAbstractionFile filename1 filesize1 hash1))  = makeAesonValueForFileUsingKnownFileHash filename1 filesize1 hash1
tAionPointToAesonValue (AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory foldername2 contents2)) = makeAesonValueForDirectoryUsingContentsHashes foldername2 contents2



