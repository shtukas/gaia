module Main where

import System.Environment
import SearchEngine
import Data.Maybe
import System.Posix.User
import ScanningAndRecordingManager

printHelp :: IO ()
printHelp = do
    putStrLn "    usage: gaia-utils run-scan-at-default-location"
    putStrLn "    usage: gaia-utils get-merkle-root"

doTheThing1 :: [String] -> IO ()
doTheThing1 args
    | (head args) == "run-scan-at-default-location" = do 
        username <- System.Posix.User.getLoginName
        ScanningAndRecordingManager.cycleMerkleRootComputationForLocation $ "/Users/"++username++"/Desktop"
        putStrLn "Done!"
    | (head args) == "get-merkle-root" = do 
        root <- SearchEngine.getMerkleRoot
        putStrLn $ fromMaybe "could not find the root!" root
    | otherwise = do 
        putStrLn ""
        putStrLn $ "I could not interpret this: "++(head args)
        printHelp
        putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    doTheThing1 args



