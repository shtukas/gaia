{-# LANGUAGE OverloadedStrings #-}

module FSRootsManagement where

import System.Environment
import System.Directory as Dir
import System.IO
import qualified Data.List
import qualified Data.Text
import qualified UserPreferences
import qualified Xcache
import qualified Data.Maybe                 as M

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

printFSRootsListing :: IO ()
printFSRootsListing = do
    filepath <- UserPreferences.getFSRootsListingFilepath
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            contents <- readFile filepath
            putStrLn "-- fs roots file ------------"
            putStr contents
            putStrLn ""
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
            putStrLn ""
            
            let oldcontents2 = Data.List.lines oldcontents1
            let oldcontents3 = filter (\line -> (Data.Text.strip $ Data.Text.pack line)/=(Data.Text.pack root )) oldcontents2
            let oldcontents4 = Data.List.unlines oldcontents3
            writeFile filepath oldcontents4

            oldcontents5 <- readFile filepath
            putStrLn "-- new file ------------"
            putStr oldcontents5
            putStrLn ""

        else do
            putStrLn "error: FSRootsListing.txt does not exist yet"
            putStrLn "       Try adding an FS Root."


-- --------------------------------------------------------------------------
-- Mapping FS Roots to Xcache Keys
-- We are going to store the Merkle roots of each FS Scan Root against the FS Scan Root
-- We then just need a map from FS Scan Roots to Xcache Keys.

xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan :: Locationpath -> String
xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath = "f9c43482-2ae6-4ecc-be23-d4b1f0c7c85d:"++locationpath

merkleRootForFSRootScan :: Locationpath -> IO ( Maybe String )
merkleRootForFSRootScan locationpath = do
    let xcachekey = xCacheStorageKeyForTheAionMerkleRootOfAFSRootScan locationpath
    Xcache.get xcachekey


