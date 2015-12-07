module Gaia.UserPreferences where

import           System.Directory as Dir
import           System.Environment (getEnv)
import           System.IO.Error (catchIOError, ioError, isDoesNotExistError)

type FolderPath = String

xcacheRepositoryLegacyFolderPath :: FolderPath
xcacheRepositoryLegacyFolderPath = "/x-space/xcache-v2"

getEnvFailback :: String -> String -> IO String
getEnvFailback env failback =
    catchIOError (getEnv env) (\e -> if isDoesNotExistError e then return failback else ioError e)

getXCacheRoot :: IO String
getXCacheRoot = getEnvFailback "GAIAXCACHEROOT" xcacheRepositoryLegacyFolderPath

getFSRootsListingFilePath :: IO String
getFSRootsListingFilePath = do
    folderpath <- Dir.getAppUserDataDirectory "gaia"
    return (folderpath ++ "/" ++ "FSRootsListing.txt")

