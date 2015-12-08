{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.SearchEngine (
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath,
    runQueryAgainMerkleRootUsingStoredData
) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.List as D
import qualified Gaia.AesonObjectsUtils as GAOU
import qualified Gaia.Directives as GD
import qualified Gaia.GeneralUtils as GU
import           Gaia.Types
import qualified PStorageServices.ContentAddressableStore as CAS
import qualified System.FilePath as FS

-- -----------------------------------------------------------

{-
    In the first section we implement the selection process.
    Currently we
        - search for occurences of substrings in location paths
        - process tags in gaia files
-}

-- -----------------------------------------------------------

--shouldRetainThisLocationPath :: LocationPath -> String -> Bool
--shouldRetainThisLocationPath locationpath pattern = pattern `D.isInfixOf` locationpath

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

casKeyToAionName :: String -> MaybeT IO String
casKeyToAionName key = do
    aionPointAsByteString <- MaybeT $ CAS.get key
    let aesonValue = GAOU.convertJSONStringIntoAesonValue (Char8.unpack aionPointAsByteString)
    returnAppropriateName aesonValue
    where
        returnAppropriateName :: Maybe A.Value -> MaybeT IO String
        returnAppropriateName Nothing = mzero
        returnAppropriateName (Just aesonValue) = 
            let tap = GAOU.aesonValueToTAionPoint aesonValue in 
            let name = if GAOU.aesonValueIsFile aesonValue then name1 tap else name2 tap in
            return name

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueFileAndPatternAndLocationPath <aesonValueFile> <search pattern> <current path>

extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> MaybeT IO [ LocationPath ]
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonValueFile pattern locationpath =
    do
        let tap = GAOU.aesonValueToTAionPoint aesonValueFile
        if name1 tap =="gaia"
            then do
                -- parseDirectivesFile :: FilePath -> MaybeT IO [Directive]
                directives <- GD.parseDirectivesFile locationpath
                if shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern directives
                    then
                        return [ FS.takeDirectory locationpath ]
                    else
                        mzero
            else
                if shouldRetainThisLocationInVirtueOfTheName (name1 tap) pattern
                    then
                        return [ locationpath ]
                    else
                        mzero

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath <aesonValueDirectory> <search pattern> <current path>

extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> MaybeT IO [ LocationPath ]
extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonValueDirectory pattern locationpath = do
    arrays <- mapM (\caskey -> do
                                name <- casKeyToAionName caskey
                                extractLocationPathsForAionCASKeyAndPatternAndLocationPath caskey pattern (FS.normalise $ FS.joinPath [locationpath, name])
                    ) caskeys
        -- [ IO ( Maybe [ LocationPath ] ) ] ~mapM~> MaybeT IO [ [ LocationPath ] ] ~> [ [ LocationPath ] ] (here it practically has the effect of catMaybes)

    return $ evtAddLocation (concat arrays)
    where
        tap = GAOU.aesonValueToTAionPoint aesonValueDirectory
        foldername = name2 tap
        caskeys = contents2 tap
        evtAddLocation array = if shouldRetainThisLocationInVirtueOfTheName foldername pattern
            then
                locationpath : array
            else
                array

-- -----------------------------------------------------------

extractLocationPathsForAesonValueAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> MaybeT IO [ LocationPath ]
extractLocationPathsForAesonValueAndPatternAndLocationPath aesonObject pattern locationpath =
    if GAOU.aesonValueIsFile aesonObject
        then extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonObject pattern locationpath
        else extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonObject pattern locationpath

-- -----------------------------------------------------------

-- extractLocationPathsForAionCASKeyAndPatternAndLocationPath <aion cas hash> <search pattern> <current path>

extractLocationPathsForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> MaybeT IO [ LocationPath ]
extractLocationPathsForAionCASKeyAndPatternAndLocationPath _ "" _ = mzero
extractLocationPathsForAionCASKeyAndPatternAndLocationPath aion_cas_hash pattern locationpath = do
    aionJSONValueAsString <- GAOU.getAionJSONStringForCASKey aion_cas_hash
    let aionJSONValue = GAOU.convertJSONStringIntoAesonValue aionJSONValueAsString
    returnAppropriatePaths aionJSONValue
    where
        returnAppropriatePaths :: Maybe A.Value -> MaybeT IO [ LocationPath ]
        returnAppropriatePaths Nothing = mzero
        returnAppropriatePaths (Just aionJSONValue) = extractLocationPathsForAesonValueAndPatternAndLocationPath aionJSONValue pattern locationpath

-- -----------------------------------------------------------

runQueryAgainMerkleRootUsingStoredData :: LocationPath -> String -> String -> MaybeT IO [ LocationPath ]
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern =
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath merkleroot pattern fsroot

