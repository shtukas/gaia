module Main where

import System.Environment
import qualified SearchEngine
import Data.Maybe
import System.Posix.User
import qualified ScanningAndRecordingManager
import qualified ContentAddressableStore

printHelp :: IO ()
printHelp = do
    putStrLn "    usage: gaia-utils run-scan-at-default-location"
    putStrLn "    usage: gaia-utils get-merkle-root"
    putStrLn "    usage: gaia-utils cas-get <key>"

doTheThing1 :: [String] -> IO ()
doTheThing1 args
    | length args == 0 = do
        putStrLn ""
        printHelp
        putStrLn ""        
    | (head args) == "run-scan-at-default-location" = do 
        username <- System.Posix.User.getLoginName
        ScanningAndRecordingManager.cycleMerkleRootComputationForLocation $ "/Users/"++username++"/Desktop"
        putStrLn "Done!"
    | (head args) == "get-merkle-root" = do 
        root <- SearchEngine.getMerkleRoot
        putStrLn $ fromMaybe "could not find the root!" root
    | ( (head args) == "cas-get" ) && ( length args >= 2 ) = do 
        let key = ( head $ drop 1 args )
        string <- ContentAddressableStore.get key
        putStrLn string 
    | otherwise = do 
        putStrLn ""
        putStrLn $ "I could not interpret this: "++(head args)
        printHelp
        putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    doTheThing1 args



