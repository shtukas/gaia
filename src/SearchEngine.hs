{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SearchEngine (
    extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath,
    runQueryAgainMerkleRootUsingStoredData
) where

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.Maybe as M

import qualified AesonObjectsUtils

type Locationpath = String

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

        let array1 = map (\k -> extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath k pattern (current_path ++ "/" ++ foldername) ) cas_keys
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

-- extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath <aion cas hash> <search pattern> <current path>

extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath :: String -> String -> String -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath _ "" _ = do
    return $ Just []
extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath aion_cas_hash pattern current_path = do
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

runQueryAgainMerkleRootUsingStoredData :: String -> String -> String -> IO ( Maybe [ Locationpath ] )
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern = do
    extractLocationpathsForAionCASHashAndQueryAndContextualFolderpath merkleroot pattern fsroot

