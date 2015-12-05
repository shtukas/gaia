{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SearchEngine (
    getMerkleRoot,
    extractLocationpathsForAionCASHashAndQuery,
    cycleQuery
) where

import qualified System.Directory as Dir

import qualified ContentAddressableStore

type Locationpath = String

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

getAeonJSONStringForCASHash :: String -> IO String
getAeonJSONStringForCASHash hash = ContentAddressableStore.get hash

-- This is the core of the search engine
-- Recursively looks for paths matching the pattern
extractLocationpathsForAionCASHashAndQuery :: String -> String -> String -> [ Locationpath ]
extractLocationpathsForAionCASHashAndQuery _ "" _ = [] -- returning empty results for empty pattern
extractLocationpathsForAionCASHashAndQuery aion_cash_hash pattern current_path = 
    let aionJSONValue = Nothing
    in []    

cycleQuery :: String -> IO [ Locationpath ]
cycleQuery pattern = 
    do
        return [ "/Users/pascal/Desktop" ]


