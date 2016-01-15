module Gaia.UserPreferences (
    getXCacheRoot,
    getFSRootsListingFilePath
) where

import           Gaia.Types
import           System.Directory as Dir
import           System.Environment (getEnv)
import           System.FilePath (normalise, (</>))
import           System.IO.Error (catchIOError, isDoesNotExistError)

xcacheRepositoryLegacyFolderPath :: FolderPath
xcacheRepositoryLegacyFolderPath = "/x-space/xcache-v2"

ensureFolderPath :: FolderPath -> IO ()
ensureFolderPath = Dir.createDirectoryIfMissing True

getEnvFailback :: String -> String -> IO String
getEnvFailback env failback =
    catchIOError (getEnv env) (\e -> if isDoesNotExistError e then return failback else ioError e)

getXCacheRoot :: IO String
getXCacheRoot = getEnvFailback "GAIAXCACHEROOT" xcacheRepositoryLegacyFolderPath

getFSRootsListingFilePath :: IO String
getFSRootsListingFilePath = do
    folderpath <- Dir.getAppUserDataDirectory "gaia"
    ensureFolderPath folderpath
    return $ normalise $ folderpath </> "FSRootsListing.txt"

