{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SearchEngine (
    extractLocationpathsForAionCASKeyAndPatternAndLocationpath,
    runQueryAgainMerkleRootUsingStoredData
) where

import qualified Data.Aeson as A
    -- A.decode :: A.FromJSON a => Char8.ByteString -> Maybe a

import qualified Data.Maybe as M

import qualified AesonObjectsUtils

import qualified ContentAddressableStore

import qualified AesonObjectsUtils

import qualified Data.List as D

import qualified Data.Char as C

import qualified Gaia.Directives as GD

import           Filesystem.Path (directory)

import           Filesystem.Path.Rules
    -- encodeString :: Rules platformFormat -> FilePath -> String
    -- decodeString :: Rules platformFormat -> String -> FilePath

import qualified Data.ByteString.Lazy.Char8 as Char8

type Locationpath = String

-- -----------------------------------------------------------

{-
    In the first section we implement the selection process. 
    Currently we 
        - search for occurences of substrings in location paths
        - process tags in gaia files
-}

-- -----------------------------------------------------------

shouldRetainThisLocationPath :: Locationpath -> String -> Bool
shouldRetainThisLocationPath locationpath pattern = D.isInfixOf pattern locationpath

-- *AesonObjectsUtils> Gaia.Directives.parseDirectivesFile "/Users/pascal/Desktop/Gifs/gaia" 
-- Right [Tag -> "Use the force, Luke"]

shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives :: Locationpath -> String -> [GD.Directive] -> Bool
shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern parsedirectives = 
    True

extractParentDirectoryFolderpath :: String -> String
extractParentDirectoryFolderpath locationpath = locationpath -- directory locationpath

shouldRetainThisLocationInVirtueOfTheName :: String -> String -> Bool
shouldRetainThisLocationInVirtueOfTheName name pattern = D.isInfixOf ( map (\c -> C.toLower c ) pattern ) ( map (\c -> C.toLower c ) name )

-- -----------------------------------------------------------

-- The two below functions are temporary

stringToFSFilepath :: String -> FilePath
stringToFSFilepath locationpath = locationpath -- decodeString "" locationpath 

fSFilepathToString :: FilePath -> String
fSFilepathToString locationpath = locationpath -- encodeString "" locationpath

-- -----------------------------------------------------------

{-
	Each Aion point is a location of the File System
	Therefore each Aeson Value is a location of the file system
	In the below <current path> is the full FS path to the object (which is not known by the object itself and must be computed recursively from some root)
-}

-- -----------------------------------------------------------

casKeyToAionName :: String -> IO String
casKeyToAionName key = do
    aionPointAsMaybeByteString <- ContentAddressableStore.get key
    case aionPointAsMaybeByteString of
        Nothing                    -> return "" -- TODO TODAY : The type of the function itself is not correct 
        Just aionPointAsByteString -> do
            let aesonValue = AesonObjectsUtils.convertJSONStringIntoAesonValue (Char8.unpack aionPointAsByteString)
            case aesonValue of
                Nothing          -> return "" -- TODO TODAY : The type of the function itself is not correct 
                Just aesonValue' -> 
                    if AesonObjectsUtils.aesonValueIsFile aesonValue'
                        then do
                            let (filename,_,_) = AesonObjectsUtils.aesonValueForFileGaiaProjection aesonValue'
                            return filename
                        else do
                            let (foldername,_) = AesonObjectsUtils.aesonValueForDirectoryGaiaProjection aesonValue'
                            return foldername

-- -----------------------------------------------------------

-- extractLocationpathsForAesonValueFileAndPatternAndLocationpath <aesonObjectFile> <search pattern> <current path>

extractLocationpathsForAesonValueFileAndPatternAndLocationpath :: A.Value -> String -> Locationpath -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAesonValueFileAndPatternAndLocationpath aesonObjectFile pattern locationpath = 
    do
        let aValue = aesonObjectFile
        let (filename, filesize, sha1shah) = AesonObjectsUtils.aesonValueForFileGaiaProjection aValue
        if filename=="gaia" 
            then do
                -- parseDirectivesFile :: Filepath -> IO (Either ParseError [Directive])
                epd <- GD.parseDirectivesFile (locationpath)
                -- Either ParseError [Directive]
                case epd of
                  Left x ->
                        return $ Just []
                  Right directives -> do
                    if shouldRetainThisLocationPathAsDirectoryGivenTheseGaiaDirectives locationpath pattern directives
                        then
                            return $ Just [ extractParentDirectoryFolderpath locationpath ]
                        else
                            return $ Just []
            else
                if shouldRetainThisLocationInVirtueOfTheName filename pattern
                    then 
                        return $ Just [locationpath]
                    else
                        return $ Just []

-- -----------------------------------------------------------

-- extractLocationpathsForAesonValueDirectoryAndPatternAndLocationpath <aesonObjectDirectory> <search pattern> <current path>

extractLocationpathsForAesonValueDirectoryAndPatternAndLocationpath :: A.Value -> String -> Locationpath -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAesonValueDirectoryAndPatternAndLocationpath aesonObjectDirectory pattern locationpath = 
    do
        let aValue = aesonObjectDirectory

        let (foldername, caskeys) = AesonObjectsUtils.aesonValueForDirectoryGaiaProjection aesonObjectDirectory 
            -- ( foldername, [CAS-Keys(s)] ) 
            -- ( String, [String] )

        let array1 = map (\caskey -> 
                            do
                                name <- casKeyToAionName caskey
                                extractLocationpathsForAionCASKeyAndPatternAndLocationpath caskey pattern (locationpath ++ "/" ++ name) 
                        ) caskeys
            -- [ IO ( Maybe [ Locationpath ] ) ]

        let array2 = sequence array1
            -- IO [ Maybe [ Locationpath ] ]

        array3 <- array2
            -- [ Maybe [ Locationpath ] ]

        let array4 = map (\x -> M.fromJust x ) array3       
             -- [ [ Locationpath ] ]

        let array5 = concat array4

        let array6 = if shouldRetainThisLocationInVirtueOfTheName foldername pattern
                        then 
                            [locationpath] ++ array5
                        else
                            array5

        return $ Just array6

-- -----------------------------------------------------------

extractLocationpathsForAesonValueAndPatternAndLocationpath :: A.Value -> String -> Locationpath -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAesonValueAndPatternAndLocationpath aesonObject pattern locationpath = 
    if AesonObjectsUtils.aesonValueIsFile aesonObject
        then do
            extractLocationpathsForAesonValueFileAndPatternAndLocationpath aesonObject pattern locationpath
        else do
            extractLocationpathsForAesonValueDirectoryAndPatternAndLocationpath aesonObject pattern locationpath

-- -----------------------------------------------------------

-- extractLocationpathsForAionCASKeyAndPatternAndLocationpath <aion cas hash> <search pattern> <current path>

extractLocationpathsForAionCASKeyAndPatternAndLocationpath :: String -> String -> Locationpath -> IO ( Maybe [ Locationpath ] )
extractLocationpathsForAionCASKeyAndPatternAndLocationpath _ "" _ = do
    return $ Just []
extractLocationpathsForAionCASKeyAndPatternAndLocationpath aion_cas_hash pattern locationpath = do
    aionJSONValueAsString <- AesonObjectsUtils.getAesonJSONStringForCASKey aion_cas_hash
    if M.isJust aionJSONValueAsString
        then do
            let aionJSONValueMaybe = AesonObjectsUtils.convertJSONStringIntoAesonValue $ M.fromJust aionJSONValueAsString
            if M.isJust aionJSONValueMaybe
                then do 
                    let aionJSONValue = M.fromJust aionJSONValueMaybe
                    extractLocationpathsForAesonValueAndPatternAndLocationpath aionJSONValue pattern locationpath 
                else
                    return Nothing    
        else do
            return Nothing

-- -----------------------------------------------------------

runQueryAgainMerkleRootUsingStoredData :: Locationpath -> String -> String -> IO ( Maybe [ Locationpath ] )
runQueryAgainMerkleRootUsingStoredData fsroot merkleroot pattern = do
    extractLocationpathsForAionCASKeyAndPatternAndLocationpath merkleroot pattern fsroot

