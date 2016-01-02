{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gaia.SearchEngine (
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath,
    runQueryAgainMerkleRootUsingStoredData,
    runQuery2
) where

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.List as D
import qualified Gaia.FSRootsManagement as FSM
import qualified Gaia.AesonObjectsUtils as GAOU
import qualified Gaia.Directives as GD
import qualified Gaia.GeneralUtils as GU
import qualified Gaia.ScanningAndRecordingManager as SRM
import           Gaia.Types
import qualified PStorageServices.ContentAddressableStore as CAS
import qualified System.FilePath as FS
import           System.IO.Unsafe (unsafePerformIO)

-- -----------------------------------------------------------

{-
    In the first section we implement the selection process.
    Currently we
        - search for occurences of substrings in location paths
        - process tags in gaia files
-}

-- -----------------------------------------------------------

shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives :: LocationPath -> [GD.GaiaFileDirective] -> String -> Bool
shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives _ parsedirectives pattern =
    any (\directive ->
            case directive of
               GaiaFileDirective GaiaFileTag body -> D.isInfixOf (GU.stringToLower pattern) (GU.stringToLower body)
        ) parsedirectives

shouldRetainThisLocationInVirtueOfTheName :: String -> String -> Bool
shouldRetainThisLocationInVirtueOfTheName name pattern = D.isInfixOf (GU.stringToLower pattern) (GU.stringToLower name)

-- -----------------------------------------------------------

{-
	Each Aion point is a location of the File System
	Therefore each Aeson Value is a location of the file system
	In the below <current path> is the full FS path to the object (which is not known by the object itself and must be computed recursively from some root)
-}

-- -----------------------------------------------------------

casKeyToAionName :: String -> IO (Maybe String)
casKeyToAionName key = do
    -- CAS.get :: String -> IO ( Maybe Char8.ByteString )
    aionPointAsByteString <- CAS.get key
    case aionPointAsByteString of 
        Nothing -> return Nothing
        Just aionPointAsByteString' -> do
            -- GAOU.convertJSONStringIntoAesonValue :: String -> Maybe A.Value
            let aesonValue = GAOU.convertJSONStringIntoAesonValue (Char8.unpack aionPointAsByteString')
            case aesonValue of 
                Nothing -> return Nothing
                Just aesonValue' -> do
                    -- aesonValueToTAionPoint :: A.Value -> TAionPoint
                    let tap = GAOU.aesonValueToTAionPoint aesonValue'
                    let name = if GAOU.aesonValueIsFile aesonValue' 
                               then name1 tap 
                               else name2 tap
                    return $ Just name

-- -----------------------------------------------------------

{-
extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO [ LocationPath ]
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonValueFile pattern locationpath =
    do
        let tap = GAOU.aesonValueToTAionPoint aesonValueFile
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
                if shouldRetainThisLocationInVirtueOfTheName (name1 tap) pattern
                    then
                        return [ locationpath ]
                    else
                        return []
-}

extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO [ LocationPath ]
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonValueFile pattern locationpath = do
        if shouldRetainThisLocationInVirtueOfTheName (name1 tap) pattern
            then
                return [ locationpath ]
            else
                return []
        where 
            tap = GAOU.aesonValueToTAionPoint aesonValueFile

-- -----------------------------------------------------------

extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO [LocationPath]
extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonValueDirectory pattern locationpath = do
    let tap = GAOU.aesonValueToTAionPoint aesonValueDirectory
    let foldername = name2 tap
    let caskeys = contents2 tap
    let x1 = map (\caskey -> do
                                maybename <- casKeyToAionName caskey
                                case maybename of
                                    Nothing -> return []
                                    -- extractLocationPathsForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> IO [LocationPath]
                                    -- extractLocationPathsForAionCASKeyAndPatternAndLocationPath <aion cas hash> <search pattern> <current path>
                                    Just name -> extractLocationPathsForAionCASKeyAndPatternAndLocationPath caskey pattern (FS.normalise $ FS.joinPath [locationpath, name])
                            ) caskeys
    -- x1 :: [IO [LocationPath]]
    let x2 = sequence x1
    -- x2 :: IO [[LocationPath]]
    let x3 = fmap concat x2
    -- x3 :: IO [LocationPath]
    if D.isInfixOf (GU.stringToLower pattern) (GU.stringToLower foldername)
    then do
        x4 <- x3
        return $ locationpath : x4
    else
        x3

-- -----------------------------------------------------------

extractLocationPathsForAesonValueAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO [LocationPath]
extractLocationPathsForAesonValueAndPatternAndLocationPath aesonObject pattern locationpath =
    if GAOU.aesonValueIsFile aesonObject
        then extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonObject pattern locationpath
        else extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonObject pattern locationpath

-- -----------------------------------------------------------

extractLocationPathsForAionCASKeyAndPatternAndLocationPath :: String -> String -> LocationPath -> IO [LocationPath]
extractLocationPathsForAionCASKeyAndPatternAndLocationPath aion_cas_hash pattern locationpath = do
    aionJSONValueAsString <- GAOU.getAionJSONStringForCASKey3 aion_cas_hash
    case aionJSONValueAsString of 
        Nothing -> return []
        Just aionJSONValueAsString' -> do
            -- GAOU.convertJSONStringIntoAesonValue :: String -> Maybe A.Value
            let aionJSONValue = GAOU.convertJSONStringIntoAesonValue aionJSONValueAsString'
            case aionJSONValue of
                Nothing -> return []
                Just aionJSONValue' -> extractLocationPathsForAesonValueAndPatternAndLocationPath aionJSONValue' pattern locationpath

-- -----------------------------------------------------------

runQueryAgainMerkleRootUsingStoredData :: LocationPath -> String -> String -> IO [ LocationPath ]
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern =
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath merkleroot pattern fsroot

-- -----------------------------------------------------------

runQuery1 :: String -> IO [String]
runQuery1 pattern = do
    -- getFSScanRoots :: IO [ String ]
    scanroots <- FSM.getFSScanRoots 
    let x = map (\scanroot -> do
                    merkleroot <- SRM.getCurrentMerkleRootForFSScanRoot scanroot
                    case merkleroot of
                        Nothing -> return []
                        Just merkleroot' -> runQueryAgainMerkleRootUsingStoredData scanroot merkleroot' pattern -- IO ( Maybe [ LocationPath ] )
                ) scanroots 
    -- x :: [ IO [ String] ]
    fmap concat ( sequence x ) 

runQuery2 :: String -> [String]
runQuery2 pattern = unsafePerformIO $ runQuery1 pattern

