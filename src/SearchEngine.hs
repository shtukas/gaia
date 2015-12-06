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

import qualified AesonObjectsUtils

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

-- extractLocationpathsForAionJsonFileObjectAndQuery <aesonObjectFile> <search pattern> <current path>

extractLocationpathsForAionJsonFileObjectAndQuery :: A.Value -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionJsonFileObjectAndQuery aesonObjectFile pattern current_path = 
    do
        let aValue = aesonObjectFile
            (filename, filesize, sha1shah) = AesonObjectsUtils.aesonValueForFileGaiaProjection aValue
        return $ Just [current_path++"/"++filename]

-- -----------------------------------------------------------

-- extractLocationpathsForAionJsonDirectoryObjectAndQuery <aesonObjectDirectory> <search pattern> <current path>

extractLocationpathsForAionJsonDirectoryObjectAndQuery :: A.Value -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionJsonDirectoryObjectAndQuery aesonObjectDirectory pattern current_path = 
    do
        let aValue = aesonObjectDirectory

        let (foldername, cas_keys) = AesonObjectsUtils.aesonValueForDirectoryGaiaProjection aesonObjectDirectory 
            -- ( foldername, [CAS-Keys(s)] ) 
            -- ( String, [String] )

        let array1 = map (\k -> extractLocationpathsForAionCASHashAndQuery k pattern (current_path ++ "/" ++ foldername) ) cas_keys
            -- [ IO ( Maybe [ Locationpath ] ) ]

        let array2 = sequence array1
            -- IO [ Maybe [ Locationpath ] ]

        array3 <- array2
            -- [ Maybe [ Locationpath ] ]

        let array4 = map (\x -> M.fromJust x ) array3       
             -- [ [ Locationpath ] ]

        let array5 = concat array4

        return $ Just array5

-- -----------------------------------------------------------

extractLocationpathsForAionJsonObjectAndQuery :: A.Value -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionJsonObjectAndQuery aesonObject pattern current_path = 
    if AesonObjectsUtils.aesonValueIsFile aesonObject
        then do
            extractLocationpathsForAionJsonFileObjectAndQuery aesonObject pattern current_path
        else do
            extractLocationpathsForAionJsonDirectoryObjectAndQuery aesonObject pattern current_path

-- -----------------------------------------------------------

-- extractLocationpathsForAionCASHashAndQuery <aion cas hash> <search pattern> <current path>

extractLocationpathsForAionCASHashAndQuery :: String -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionCASHashAndQuery _ "" _ = do
    return $ Just []
extractLocationpathsForAionCASHashAndQuery aion_cas_hash pattern current_path = do
    aionJSONValueAsString <- AesonObjectsUtils.getAesonJSONStringForCASKey aion_cas_hash
    if M.isJust aionJSONValueAsString
        then do
            let aionJSONValueMaybe = AesonObjectsUtils.convertJSONStringIntoAesonJSONObject $ M.fromJust aionJSONValueAsString
            if M.isJust aionJSONValueMaybe
                then do 
                    let aionJSONValue = M.fromJust aionJSONValueMaybe
                    extractLocationpathsForAionJsonObjectAndQuery aionJSONValue pattern current_path 
                else
                    return Nothing    
        else do
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

