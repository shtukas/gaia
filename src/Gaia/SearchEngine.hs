{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Gaia.SearchEngine (
    extractLocationPathsForAionCASKeyAndPatternAndLocationPath,
    runQueryAgainMerkleRootUsingStoredData
) where

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.Maybe as M

import qualified PStorageServices.ContentAddressableStore as CAS

import qualified Gaia.AesonObjectsUtils as GAOU

import qualified Data.List as D

import qualified Data.Char as C

import qualified Gaia.Directives as GD

import           Filesystem.Path (directory)

import           Filesystem.Path.Rules
    -- encodeString :: Rules platformFormat -> FilePath -> String
    -- decodeString :: Rules platformFormat -> String -> FilePath

import qualified Data.ByteString.Lazy.Char8 as Char8

type LocationPath = String

-- -----------------------------------------------------------

{-
    In the first section we implement the selection process. 
    Currently we 
        - search for occurences of substrings in location paths
        - process tags in gaia files
-}

-- -----------------------------------------------------------

shouldRetainThisLocationPath :: LocationPath -> String -> Bool
shouldRetainThisLocationPath locationpath pattern = D.isInfixOf pattern locationpath

-- *AesonObjectsUtils> Gaia.Directives.parseDirectivesFile "/Users/pascal/Desktop/Gifs/gaia" 
-- Right [Tag -> "Use the force, Luke"]

shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives :: LocationPath -> String -> [GD.Directive] -> Bool
shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern parsedirectives = 
    True

extractParentDirectoryFolderPath :: String -> String
extractParentDirectoryFolderPath locationpath = locationpath -- directory locationpath

shouldRetainThisLocationInVirtueOfTheName :: String -> String -> Bool
shouldRetainThisLocationInVirtueOfTheName name pattern = D.isInfixOf ( map (\c -> C.toLower c ) pattern ) ( map (\c -> C.toLower c ) name )

-- -----------------------------------------------------------

{-
	The two below functions are temporary
	TODO: Update them 
-}

stringToFSFilePath :: String -> FilePath
stringToFSFilePath locationpath = locationpath -- decodeString "" locationpath 

fSFilePathToString :: FilePath -> String
fSFilePathToString locationpath = locationpath -- encodeString "" locationpath

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
                Just aesonValue' -> 
                    if GAOU.aesonValueIsFile aesonValue'
                        then do
                            let (filename,_,_) = GAOU.aesonValueForFileGaiaProjection aesonValue'
                            return $ Just filename
                        else do
                            let (foldername,_) = GAOU.aesonValueForDirectoryGaiaProjection aesonValue'
                            return $ Just foldername

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueFileAndPatternAndLocationPath <aesonObjectFile> <search pattern> <current path>

extractLocationPathsForAesonValueFileAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAesonValueFileAndPatternAndLocationPath aesonObjectFile pattern locationpath = 
    do
        let aValue = aesonObjectFile
        let (filename, filesize, sha1shah) = GAOU.aesonValueForFileGaiaProjection aValue
        if filename=="gaia" 
            then do
                -- parseDirectivesFile :: FilePath -> IO (Either ParseError [Directive])
                epd <- GD.parseDirectivesFile (locationpath)
                -- Either ParseError [Directive]
                case epd of
                  Left x ->
                        return $ Just []
                  Right directives -> do
                    if shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern directives
                        then
                            return $ Just [ extractParentDirectoryFolderPath locationpath ]
                        else
                            return $ Just []
            else
                if shouldRetainThisLocationInVirtueOfTheName filename pattern
                    then 
                        return $ Just [locationpath]
                    else
                        return $ Just []

-- -----------------------------------------------------------

-- extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath <aesonObjectDirectory> <search pattern> <current path>

extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath :: A.Value -> String -> LocationPath -> IO ( Maybe [ LocationPath ] )
extractLocationPathsForAesonValueDirectoryAndPatternAndLocationPath aesonObjectDirectory pattern locationpath = 
    do
        let aValue = aesonObjectDirectory

        let (foldername, caskeys) = GAOU.aesonValueForDirectoryGaiaProjection aesonObjectDirectory 
            -- ( foldername, [CAS-Keys(s)] ) 
            -- ( String, [String] )

        let array1 = map (\caskey -> 
                            do
                                name' <- casKeyToAionName caskey
                                case name' of 
                                        Nothing   -> return Nothing
                                        Just name -> extractLocationPathsForAionCASKeyAndPatternAndLocationPath caskey pattern (locationpath ++ "/" ++ name)
                        ) caskeys
            -- [ IO ( Maybe [ LocationPath ] ) ]

        let array2 = sequence array1
            -- IO [ Maybe [ LocationPath ] ]

        array3 <- array2
            -- [ Maybe [ LocationPath ] ]

        let array4 = map (\x -> 
                            case x of
                                Nothing -> []
                                Just x' -> x' 
                        ) array3       
             -- [ [ LocationPath ] ]

        let array5 = concat array4

        let array6 = if shouldRetainThisLocationInVirtueOfTheName foldername pattern
                        then 
                            [locationpath] ++ array5
                        else
                            array5

        return $ Just array6

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
    aionJSONValueAsString' <- GAOU.getAesonJSONStringForCASKey aion_cas_hash
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

