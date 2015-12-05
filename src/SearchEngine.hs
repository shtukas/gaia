{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SearchEngine (
    getMerkleRoot,
    extractLocationpathsForAionCASHashAndQuery,
    runQueryAgainMerkleRootUsingStoredData
) where

import qualified System.Directory as Dir

import qualified ContentAddressableStore

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Data.Maybe as M

import qualified Data.HashMap.Strict as HM

import qualified AeonObjectsUtils

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

-- extractLocationpathsForAionJsonFileObjectAndQuery <aeonObjectFile> <search pattern> <current path>

extractLocationpathsForAionJsonFileObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonFileObjectAndQuery aeonObjectFile pattern current_path = 
    let aValue = aeonObjectFile
        (filename, filesize, sha1shah) = AeonObjectsUtils.extractGaiaDataFromAeonValueForFile aValue
    in Just [filename]

-- -----------------------------------------------------------

-- extractLocationpathsForAionJsonDirectoryObjectAndQuery <aeonObjectDirectory> <search pattern> <current path>

extractLocationpathsForAionJsonDirectoryObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonDirectoryObjectAndQuery aeonObjectDirectory pattern current_path = 
    let aValue = aeonObjectDirectory
        (foldername, cas_keys) = AeonObjectsUtils.extractGaiaDataFromAeonValueForDirectory aValue
    in Just [foldername]

-- -----------------------------------------------------------

extractLocationpathsForAionJsonObjectAndQuery :: A.Value -> String -> String -> Maybe [ Locationpath ]
extractLocationpathsForAionJsonObjectAndQuery aeonObject pattern current_path = 
    let 
        value1 = AeonObjectsUtils.extractListOfPairsFromAeonValueObject aeonObject
        value2 = Prelude.lookup "aion-type" ( M.fromJust value1 )
        value3 = M.fromJust value2
        value4 = AeonObjectsUtils.extractUnderlyingTextFromAeonValueString value3
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
    let aionJSONValueMaybe = AeonObjectsUtils.convertJSONStringIntoAeonJSONObject aionJSONValueAsString
    if M.isJust aionJSONValueMaybe
        then do 
            let aionJSONValue = M.fromJust aionJSONValueMaybe
            return $ extractLocationpathsForAionJsonObjectAndQuery aionJSONValue pattern current_path 
        else
            return Nothing    

-- -----------------------------------------------------------

runQueryAgainMerkleRootUsingStoredData :: String -> IO ( Maybe [ Locationpath ] )
runQueryAgainMerkleRootUsingStoredData pattern = do
    merkleroot <- getMerkleRoot
    if M.isJust merkleroot
        then
            do extractLocationpathsForAionCASHashAndQuery ( M.fromJust merkleroot ) pattern "/Users/pascal/Desktop"
        else
            return Nothing

