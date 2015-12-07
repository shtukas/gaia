{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.SearchEngine (
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath,
    runQueryAgainMerkleRootUsingStoredData
) where

import qualified Data.Aeson                               as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a
import qualified Data.ByteString.Lazy.Char8               as Char8
import qualified Data.List                                as D
import           Data.Maybe                               (catMaybes)
import qualified Gaia.AesonObjectsUtils                   as GAOU
import qualified Gaia.Directives                          as GD
import qualified Gaia.GeneralUtils                        as GU
import           Gaia.Types
import qualified PStorageServices.ContentAddressableStore as CAS
import qualified System.FilePath                          as FS

-- -----------------------------------------------------------

{-
    In the first section we implement the selection process. 
    Currently we 
        - search for occurences of substrings in location paths
        - process tags in gaia files
-}

-- -----------------------------------------------------------

shouldRetainThisLocationPath :: LocationPath -> String -> Bool
shouldRetainThisLocationPath locationpath pattern = pattern `D.isInfixOf` locationpath

-- *AesonObjectsUtils> Gaia.Directives.parseDirectivesFile "/Users/pascal/Desktop/Gifs/gaia" 
-- Right [Tag -> "Use the force, Luke"]

shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives :: LocationPath -> String -> [GD.GaiaFileDirective] -> Bool
shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives _ pattern parsedirectives = 
    any (\directive -> 
            case directive of
               GaiaFileDirective GaiaFileTag body -> ( GU.stringToLower pattern ) `D.isInfixOf` ( GU.stringToLower body )
        ) parsedirectives

shouldRetainThisLocationInVirtueOfTheName :: String -> String -> Bool
shouldRetainThisLocationInVirtueOfTheName name pattern = ( GU.stringToLower pattern ) `D.isInfixOf` ( GU.stringToLower name )

-- -----------------------------------------------------------

{-
	Each Aion point is a location of the File System
	Therefore each Aeson Value is a location of the file system
	In the below <current path> is the full FS path to the object (which is not known by the object itself and must be computed recursively from some root)
-}

-- -----------------------------------------------------------

casKeyToAionName :: String -> IO ( Maybe String )
casKeyToAionName key = do
    aionPointAsMaybeByteString <- CAS.get key
    case aionPointAsMaybeByteString of
        Nothing                    -> return Nothing
        Just aionPointAsByteString -> do
            let aesonValue = GAOU.convertJSONStringIntoAesonValue (Char8.unpack aionPointAsByteString)
            case aesonValue of
                Nothing          -> return Nothing 
                Just aesonValue' -> do
                    if GAOU.aesonValueIsFile aesonValue'
                        then do
                            let tap = GAOU.aesonValueToTAionPoint aesonValue'
                            return $ Just ( name1 tap )
                        else do
                            let tap = GAOU.aesonValueToTAionPoint aesonValue'
                            return $ Just ( name1 tap )

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueFileAndPatternAndLocationPath <aesonValueFile> <search pattern> <current path>

extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonValueFile pattern locationpath = 
    do
        let tap = GAOU.aesonValueToTAionPoint aesonValueFile
        if (name1 tap)=="gaia"
            then do
                -- parseDirectivesFile :: FilePath -> IO (Either ParseError [Directive])
                epd <- GD.parseDirectivesFile locationpath
                -- Either ParseError [Directive]
                case epd of
                  Left _ ->
                        return $ Just []
                  Right directives -> do
                    if shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern directives
                        then
                            return $ Just [ FS.takeDirectory locationpath ]
                        else
                            return $ Just []
            else
                if shouldRetainThisLocationInVirtueOfTheName (name1 tap) pattern
                    then 
                        return $ Just [locationpath]
                    else
                        return $ Just []

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath <aesonValueDirectory> <search pattern> <current path>

extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonValueDirectory pattern locationpath = 
    do 
        let tap = GAOU.aesonValueToTAionPoint aesonValueDirectory
        let foldername = ( name2 tap )
        let caskeys = ( contents2 tap )
        arrays <- mapM (\caskey -> do
                                    name' <- casKeyToAionName caskey
                                    case name' of
                                        Nothing   -> return Nothing
                                        Just name -> extractLocationPathsForAionCASKeyAndPatternAndLocationPath caskey pattern (FS.normalise $ FS.joinPath [locationpath, name])
                        ) caskeys
            -- [ IO ( Maybe [ LocationPath ] ) ] ~mapM~> IO [ Maybe [ LocationPath ] ] ~> [ Maybe [ LocationPath ] ]

        let array = concat $ catMaybes arrays
             -- [ Maybe [ LocationPath ] ] ~catMaybes~> [ [ LocationPath ] ] ~> [LocationPath]

        let array' = if shouldRetainThisLocationInVirtueOfTheName foldername pattern
            then
                locationpath : array
            else
                array

        return $ Just array'

-- -----------------------------------------------------------

extractLocationPathsForAesonValueAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAesonValueAndPatternAndLocationPath aesonObject pattern locationpath = 
    if GAOU.aesonValueIsFile aesonObject
        then do
            extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonObject pattern locationpath
        else do
            extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonObject pattern locationpath

-- -----------------------------------------------------------

-- extractLocationPathsForAionCASKeyAndPatternAndLocationPath <aion cas hash> <search pattern> <current path>

extractLocationPathsForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAionCASKeyAndPatternAndLocationPath _ "" _ = do
    return $ Just []
extractLocationPathsForAionCASKeyAndPatternAndLocationPath aion_cas_hash pattern locationpath = do
    aionJSONValueAsString' <- GAOU.getAionJSONStringForCASKey aion_cas_hash
    case aionJSONValueAsString' of
        Nothing                    -> return Nothing
        Just aionJSONValueAsString -> do
            let aionJSONValueMaybe = GAOU.convertJSONStringIntoAesonValue aionJSONValueAsString
            case aionJSONValueMaybe of
                Nothing                 -> return Nothing 
                Just aionJSONValue -> do  
                    extractLocationPathsForAesonValueAndPatternAndLocationPath aionJSONValue pattern locationpath

-- -----------------------------------------------------------

runQueryAgainMerkleRootUsingStoredData :: LocationPath -> String -> String -> IO ( Maybe [ LocationPath ] )
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern = do
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath merkleroot pattern fsroot

