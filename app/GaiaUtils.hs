module Main where

import System.Environment
import qualified SearchEngine
import qualified ScanningAndRecordingManager
import qualified ContentAddressableStore
import qualified Data.Maybe as M
import qualified AesonObjectsUtils
import qualified SystemIntegrity
import qualified FSRootsManagement

printHelp :: IO ()
printHelp = do
    putStrLn "    usage: gaia-utils general-scan"
    putStrLn "    usage: gaia-utils get-merkle-roots"
    putStrLn "    usage: gaia-utils cas-get <key>"
    putStrLn "    usage: gaia-utils expose-aeson-object <key>"
    putStrLn "    usage: gaia-utils run-query <pattern>"
    putStrLn "    usage: gaia-utils fsck"
    putStrLn "    usage: gaia-utils status-report"
    putStrLn "    usage: gaia-utils print-fs-roots"
    putStrLn "    usage: gaia-utils add-fs-root <locationpath>"
    putStrLn "    usage: gaia-utils remove-fs-root <locationpath>"

doTheThing1 :: [String] -> IO ()
doTheThing1 args

    | length args == 0 = do
        putStrLn ""
        printHelp
        putStrLn ""        

    | (head args) == "general-scan" = do
        ScanningAndRecordingManager.generalScan

    | (head args) == "get-merkle-roots" = do
        scanroots <- FSRootsManagement.getFSScanRoots
        _ <- sequence $ map ( \scanroot -> do 
                            putStrLn $ "location: "++scanroot
                            merkle <- ScanningAndRecordingManager.getCurrentMerkleRootForFSScanRoot scanroot -- IO ( Maybe String )
                            if M.isJust merkle
                                then do
                                    putStrLn $ "merkle  : "++(M.fromJust merkle) 
                                else
                                    putStrLn "merkle  : Unknown!"
                       ) scanroots
        return ()

    | ( (head args) == "cas-get" ) && ( length args >= 2 ) = do 
        let key = ( head $ drop 1 args )
        string <- ContentAddressableStore.get key
        if M.isJust string
            then do
                putStrLn $ M.fromJust string 
            else
                putStrLn "error: Could not retrive data for this key"

    | ( (head args) == "expose-aeson-object" ) && ( length args >= 2 ) = do
        let aion_cas_hash = ( head $ drop 1 args )
        aionJSONValueAsString <- ContentAddressableStore.get aion_cas_hash
        if M.isJust aionJSONValueAsString
            then
                if ( length $ M.fromJust aionJSONValueAsString ) == 0
                    then
                        putStrLn "I could not find a ContentAddressableStore record"  
                    else    
                        do 
                            let aionJSONValueMaybe = AesonObjectsUtils.convertJSONStringIntoAesonJSONObject $ M.fromJust aionJSONValueAsString
                            if M.isJust aionJSONValueMaybe
                                then do 
                                    let aionJSONValue = M.fromJust aionJSONValueMaybe
                                    putStrLn $ show aionJSONValue
                                else
                                    putStrLn "I could not convert the record to a Aeson Object"  
            else 
                putStrLn "error: Could not retrive data for this key" 
        return ()

    | ( (head args) == "run-query" ) && ( length args >= 2 ) = do
        let pattern = ( head $ drop 1 args )
        scanroots <- FSRootsManagement.getFSScanRoots
        _ <- sequence $ map ( \scanroot -> do 
                            putStrLn scanroot
                            merkleroot <- ScanningAndRecordingManager.getCurrentMerkleRootForFSScanRoot scanroot
                            if M.isJust merkleroot
                                then do
                                    locationpaths <- SearchEngine.runQueryAgainMerkleRootUsingStoredData scanroot ( M.fromJust merkleroot ) pattern -- IO ( Maybe [ Locationpath ] )
                                    -- Maybe [ Locationpath ]
                                    if M.isJust locationpaths
                                        then do 
                                            let folderpaths = M.fromJust locationpaths
                                            _ <- sequence $ map (\folderpath -> putStrLn ( "    " ++ folderpath)) folderpaths
                                            return ()
                                        else 
                                            putStrLn "error: Query has failed (for some reasons...)"
                                else
                                    putStrLn "error: Could not retrieve Merkle root for this location"

                       ) scanroots
        return ()

    | (head args) == "fsck" = do
        scanroots <- FSRootsManagement.getFSScanRoots
        _ <- sequence $ map ( \scanroot -> do 
                            putStrLn scanroot
                            merkleroot <- ScanningAndRecordingManager.getCurrentMerkleRootForFSScanRoot scanroot
                            if M.isJust merkleroot
                                then do
                                    bool <- SystemIntegrity.aionTreeFsckCASKey ( M.fromJust merkleroot )
                                    if bool
                                        then putStrLn "Aion Tree is correct"
                                        else putStrLn "error: Aion Tree does not check out"
                                else
                                    putStrLn "error: Could not retrieve Merkle root for this location"

                       ) scanroots
        return ()

    | (head args) == "status-report" = do
        roots <- FSRootsManagement.getFSScanRoots
        putStr "FS Scan Root file has "
        putStr $ show $ length roots
        putStrLn " elements"

    | (head args) == "print-fs-roots" = do
        FSRootsManagement.printFSRootsListing

    | ( (head args) == "add-fs-root" ) && ( length args >= 2 ) = do
        let fsroot = ( head $ drop 1 args )
        FSRootsManagement.addFSRoot fsroot       

    | ( (head args) == "remove-fs-root" ) && ( length args >= 2 ) = do
        let fsroot = ( head $ drop 1 args )
        FSRootsManagement.removeFSRoot fsroot  

    | otherwise = do 
        putStrLn ""
        putStrLn $ "I could not interpret this: "++(head args)
        printHelp
        putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    doTheThing1 args


{-|

    Extracted using expose-aeson-object

    Object (
        fromList [
            (
                "contents",Array (
                    fromList [
                        String "f2debbc0395676188af9224f21beebde4dfde586",
                        String "ac5b36985f766835d0e43d365d60ad3f242e0d04",
                        String "bbbb65c8fdbb5c24ae960ec832f8f3c72c6ed5f3",
                        String "74060dab7e3754a7e698b878a23540283b254971",
                        String "684ea29239107360c1a96e594810bc4235caf288"
                    ]
                )
            ),
            ("aion-type",String "directory"),
            ("name",String "Desktop"),
            ("version",Number 1.0)
        ]
    )

	Object (
		fromList [
			("hash",String "49f83f1f31fb9605a6d22f81acd59a7b39a40e4b"),
			("size",Number 2143190.0),("aion-type",String "file"),
			("name",String "1449085780693.jpg"),
			("version",Number 1.0)
		]
	)


-}


