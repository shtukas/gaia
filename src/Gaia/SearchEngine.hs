{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.SearchEngine (
    runQuery2,
    runQuery3
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.List as D
import qualified Gaia.AesonValuesFileSystemCorrespondance as XP1
import qualified Gaia.AesonValuesAionPointAbstractionsCorrespondance as XP2
import qualified Gaia.Directives as GD
import qualified Gaia.FSRootsManagement as FSM
import qualified Gaia.GeneralUtils as GU
import qualified Gaia.ScanningAndRecordingManager as SRM
import           Gaia.Types
import qualified PStorageServices.ContentAddressableStore as CAS
import qualified System.FilePath as FS
import           System.IO.Unsafe (unsafePerformIO)

-- -----------------------------------------------------------
-- Utils 
-- -----------------------------------------------------------

isInfixOfCaseIndependent2 :: String -> String -> Bool
isInfixOfCaseIndependent2 pattern name = D.isInfixOf (GU.stringToLower pattern) (GU.stringToLower name)

shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives :: LocationPath -> [GD.GaiaFileDirective] -> String -> Bool
shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives _ parsedirectives pattern =
    any (\directive ->
            case directive of
               GaiaFileDirective GaiaFileTag body -> isInfixOfCaseIndependent2 pattern body
        ) parsedirectives

casKeyToAionName :: String -> IO (Maybe String)
casKeyToAionName key = do
    aionPointAsByteString <- CAS.get key
    case aionPointAsByteString of 
        Nothing -> return Nothing
        Just aionPointAsByteString' -> do
            let aesonValue = XP1.convertJSONStringIntoAesonValue (Char8.unpack aionPointAsByteString')
            case aesonValue of 
                Nothing -> return Nothing
                Just aesonValue' -> do
                    let x1 = XP2.extendedAesonValueToExtendedAionPointAbstractionGeneric (ExtendedAesonValue aesonValue' key)
                    return $ Just (extractNameFromAionExtendedPointAbstractionGeneric x1)
                    where 
                        extractNameFromAionExtendedPointAbstractionGeneric (ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromFile (AionPointAbstractionFile filename _ _)) _) = filename
                        extractNameFromAionExtendedPointAbstractionGeneric (ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory foldername _)) _) = foldername

-- -----------------------------------------------------------
-- Aion Points Recursive Analysis
-- -----------------------------------------------------------

{-
	Each Aion point is a location of the File System
	Therefore each Aeson Value is a location of the file system
	In the below <current path> is the full FS path to the object (which is not known by the object itself and must be computed recursively from some root)
-}

{-
extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO [ LocationPath ]
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonValueFile pattern locationpath =
    do
        let tap = XP2.extendedAesonValueToExtendedAionPointAbstractionGeneric aesonValueFile
        if name1 tap =="gaia"
            then do
                -- parseDirectivesFile :: FilePath -> MaybeT IO [Directive]
                directives <- GD.parseDirectivesFile locationpath
                if shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath directives pattern
                    then
                        return [ FS.takeDirectory locationpath ]
                    else
                        return []
            else
                if isInfixOfCaseIndependent2 pattern (name1 tap) 
                    then
                        return [ locationpath ]
                    else
                        return []
-}

extractLocationPathsForAionPointAbstractionFileAndPatternAndLocationPath :: AionPointAbstractionFile -> String -> LocationPath -> String -> IO [SEAtom]
extractLocationPathsForAionPointAbstractionFileAndPatternAndLocationPath (AionPointAbstractionFile filename _ _) pattern locationpath caskey = do
        if isInfixOfCaseIndependent2 pattern filename
            then
                return [ SEAtom locationpath caskey ]
            else
                return []

extractLocationPathsForAionPointAbstractionDirectoryAndPatternAndLocationPath :: AionPointAbstractionDirectory -> String -> LocationPath -> String -> IO [SEAtom]
extractLocationPathsForAionPointAbstractionDirectoryAndPatternAndLocationPath (AionPointAbstractionDirectory foldername caskeys) pattern locationpath caskey = do
    let x1 = map (\xcaskey -> do
                                maybename <- casKeyToAionName xcaskey
                                case maybename of
                                    Nothing -> return []
                                    -- extractSEAtomForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> IO [LocationPath]
                                    -- extractSEAtomForAionCASKeyAndPatternAndLocationPath <aion cas hash> <search pattern> <current path>
                                    Just name -> extractSEAtomForAionCASKeyAndPatternAndLocationPath xcaskey pattern (FS.normalise $ FS.joinPath [locationpath, name])
                            ) caskeys
    -- x1 :: [IO [SEAtom]]
    let x2 = sequence x1
    -- x2 :: IO [[SEAtom]]
    let x3 = fmap concat x2
    -- x3 :: IO [SEAtom]
    if isInfixOfCaseIndependent2 pattern foldername
    then do
        x4 <- x3
        return $ (SEAtom locationpath caskey) : x4
    else
        x3

extractLocationPathsForAionPointAbstractionGenericAndPatternAndLocationPath2 :: ExtendedAionPointAbstractionGeneric -> String -> LocationPath -> IO [SEAtom]
extractLocationPathsForAionPointAbstractionGenericAndPatternAndLocationPath2 (ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromFile taionpointfile) caskey) pattern locationpath = extractLocationPathsForAionPointAbstractionFileAndPatternAndLocationPath taionpointfile pattern locationpath caskey
extractLocationPathsForAionPointAbstractionGenericAndPatternAndLocationPath2 (ExtendedAionPointAbstractionGeneric (AionPointAbstractionGenericFromDirectory taionpointdirectory) caskey) pattern locationpath = extractLocationPathsForAionPointAbstractionDirectoryAndPatternAndLocationPath taionpointdirectory pattern locationpath caskey

extractSEAtomForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> IO [SEAtom]
extractSEAtomForAionCASKeyAndPatternAndLocationPath aion_cas_hash pattern locationpath = do
    aionJSONValueAsString <- XP1.getAionJSONStringForCASKey3 aion_cas_hash
    case aionJSONValueAsString of 
        Nothing -> return []
        Just aionJSONValueAsString' -> do
            let aionJSONValue = XP1.convertJSONStringIntoAesonValue aionJSONValueAsString'
            case aionJSONValue of
                Nothing -> return []
                Just aionJSONValue' -> extractLocationPathsForAionPointAbstractionGenericAndPatternAndLocationPath2 (XP2.extendedAesonValueToExtendedAionPointAbstractionGeneric (ExtendedAesonValue aionJSONValue' aion_cas_hash)) pattern locationpath

-- -----------------------------------------------------------
-- Running queries against Merkle Roots
-- -----------------------------------------------------------

{-

	The Merkle root refers to a particular aion snapshot of the file system.

	The fsroot is used to recursively construct paths.
		Paths returned by the process are essentially the fsroot and recursively locationnames constructed by looking up the aion points.

	This is due to the fact that the Merkle root doesn't know which node of the file system (fsroot) it represents.

	The pattern is the search query.	

-}

runQueryAgainMerkleRootUsingStoredData :: LocationPath -> String -> String -> IO [SEAtom]
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern =
    extractSEAtomForAionCASKeyAndPatternAndLocationPath merkleroot pattern fsroot

-- -----------------------------------------------------------
-- Search Engine Interface
-- -----------------------------------------------------------

runQuery1 :: String -> IO [SEAtom]
runQuery1 pattern = do
    -- getFSScanRoots :: IO [String]
    scanroots <- FSM.getFSScanRoots 
    let x = map (\scanroot -> do
                    merkleroot <- SRM.getCurrentMerkleRootForFSScanRoot scanroot
                    case merkleroot of
                        Nothing -> return []
                        Just merkleroot' -> runQueryAgainMerkleRootUsingStoredData scanroot merkleroot' pattern
                ) scanroots 
    -- x :: [IO [SEAtom]]
    fmap concat ( sequence x ) 

runQuery2 :: String -> [SEAtom]
runQuery2 pattern = unsafePerformIO $ runQuery1 pattern

runQuery3 :: String -> [SEAtom]
runQuery3 pattern = unsafePerformIO $ runQuery1 pattern


