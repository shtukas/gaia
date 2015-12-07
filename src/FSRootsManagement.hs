{-# LANGUAGE OverloadedStrings #-}

module FSRootsManagement where

import           System.Directory as Dir
import qualified Data.List
import qualified Data.Text
import qualified UserPreferences
import qualified Xcache
import qualified Data.ByteString.Lazy.Char8 as Char8

type Locationpath = String

-- --------------------------------------------------------------------------
-- Managing ~/.gaia/FSRootsListing.txt

addFSRoot :: String -> IO ()
addFSRoot locationpath = do
    filepath <- UserPreferences.getFSRootsListingFilepath
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            appendFile filepath (locationpath++ "\n")
        else do
            writeFile filepath (locationpath++ "\n")
    printFSRootsListing

printFSRootsListing :: IO ()
printFSRootsListing = do
    filepath <- UserPreferences.getFSRootsListingFilepath
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            contents <- readFile filepath
            putStrLn "-- fs roots file ------------"
            putStr contents
        else do
            putStrLn "error: FSRootsListing.txt does not exist yet"
            putStrLn "       Try adding an FS Root."

removeFSRoot :: String -> IO ()
removeFSRoot root = do
    filepath <- UserPreferences.getFSRootsListingFilepath
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            oldcontents1 <- readFile filepath

            -- The next three lines only exist to consume the entire file 
            -- Otherwise the lazy read keeps a lock and the writeFile fails
            putStrLn "-- old file ------------"
            putStr oldcontents1
            
            let oldcontents2 = Data.List.lines oldcontents1
            let oldcontents3 = filter (\line -> (Data.Text.strip $ Data.Text.pack line)/=(Data.Text.pack root )) oldcontents2
            let oldcontents4 = Data.List.unlines oldcontents3
            writeFile filepath oldcontents4

            oldcontents5 <- readFile filepath
            putStrLn "-- new file ------------"
            putStr oldcontents5

        else do
            putStrLn "error: FSRootsListing.txt does not exist yet"
            putStrLn "       Try adding an FS Root."

-- --------------------------------------------------------------------------

getFSScanRoots :: IO [ String ]
getFSScanRoots = do
    filepath <- UserPreferences.getFSRootsListingFilepath
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            contents1 <- readFile filepath
            let contents2 = Data.List.lines contents1
            let contents3 = filter (\line -> (length line)>0 ) contents2
            return contents3
        else do
            return []

-- --------------------------------------------------------------------------
-- Mapping FS Roots to Xcache Keys
-- We are going to store the Merkle roots of each FS Scan Root against the FS Scan Root
-- We then just need a map from FS Scan Roots to Xcache Keys.

xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan :: Locationpath -> String
xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath = "f9c43482-2ae6-4ecc-be23-d4b1f0c7c85d:"++locationpath

merkleRootForFSRootScan :: Locationpath -> IO ( Maybe String )
merkleRootForFSRootScan locationpath = do
    let xcachekey = xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath
    merkleroot <- Xcache.get xcachekey 
    case merkleroot of 
    	Nothing   -> return Nothing
    	Just root -> return $ Just ( Char8.unpack root )


