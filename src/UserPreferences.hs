module UserPreferences where

import           System.Directory as Dir
import           System.Environment         (getEnv)
import           System.IO.Error            (catchIOError, ioError,
                                             isDoesNotExistError)

type Filepath = String
type Folderpath = String

xcacheRepositoryLegacyFolderpath :: Folderpath
xcacheRepositoryLegacyFolderpath = "/x-space/xcache-v2"

getEnvFailback :: String -> String -> IO String
getEnvFailback env failback =
    catchIOError (getEnv env) (\e -> if isDoesNotExistError e then return failback else ioError e)

getXCacheRoot :: IO String
getXCacheRoot = getEnvFailback "GAIAXCACHEROOT" xcacheRepositoryLegacyFolderpath

getFSRootsListingFilepath :: IO String
getFSRootsListingFilepath = do
    folderpath <- Dir.getAppUserDataDirectory "gaia"
    return (folderpath ++ "/" ++ "FSRootsListing.txt")

