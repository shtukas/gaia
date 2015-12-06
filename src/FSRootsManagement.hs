{-# LANGUAGE OverloadedStrings #-}

module FSRootsManagement where

import System.Environment
import System.Directory as Dir
import System.IO
import qualified Data.List
import qualified Data.Text

getFSRootsListingFilePaths :: IO String
getFSRootsListingFilePaths = do
    folderpath <- Dir.getAppUserDataDirectory "gaia"
    let filepath = folderpath ++ "/" ++ "FSRootsListing.txt"
    return filepath

addFSRoot :: String -> IO ()
addFSRoot locationpath = do
    filepath <- getFSRootsListingFilePaths
    fileexists <- Dir.doesFileExist filepath
    if fileexists
        then do
            appendFile filepath (locationpath++ "\n")
        else do
            writeFile filepath (locationpath++ "\n")

printFSRootsListing :: IO ()
printFSRootsListing = do
    filepath <- getFSRootsListingFilePaths
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
    filepath <- getFSRootsListingFilePaths
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




