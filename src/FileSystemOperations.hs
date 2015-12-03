module FileSystemOperations (
    computeMerkleRootForLocation
) where

import qualified System.Directory           as Dir
-- doesDirectoryExist :: FilePath -> IO Bool
-- getDirectoryContents :: FilePath -> IO [FilePath]

type Filepath = String
type Folderpath = String
type Locationpath = String

makeAionObjectForFilepath :: Filepath -> IO String
makeAionObjectForFilepath filepath = do
    return "{}"

makeAionObjectForDirectory :: Folderpath -> IO String
makeAionObjectForDirectory folderpath = do
    return "{}"

computeMerkleRootForLocation :: Locationpath -> IO [String]
computeMerkleRootForLocation locationpath = do
    exists <- Dir.doesDirectoryExist locationpath
    if exists 
        then do
            -- Here we have a Directory
            contents <- Dir.getDirectoryContents locationpath
            return contents
        else do
            -- Here we have a File
            return [""]



