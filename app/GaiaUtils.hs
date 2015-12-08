module Main where

import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Gaia.AesonObjectsUtils as AOU
import qualified Gaia.FSRootsManagement as FSM
import qualified Gaia.ScanningAndRecordingManager as SRM
import qualified Gaia.SearchEngine as SE
import qualified Gaia.SystemIntegrity as SI
import qualified PStorageServices.ContentAddressableStore as CAS
import           System.Environment


printHelp :: IO ()
printHelp = do
    putStrLn ""
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
    putStrLn ""

doTheThing1 :: [String] -> IO ()
doTheThing1 args
    |  null args = printHelp
    |  head args == "general-scan" = doGeneralScan
    |  head args == "get-merkle-roots" = doGetMerkleRoots
    | (head args == "cas-get") && nonemptyTail args = doCasGet (args !! 1)
    | (head args == "expose-aeson-object") && nonemptyTail args = doExposeAesonObject (args !! 1)
    | (head args == "run-query") && nonemptyTail args = doRunQuery (args !! 1)
    |  head args == "fsck" = doFsck 
    |  head args == "status-report" = doStatusReport
    |  head args == "print-fs-roots" = doPrintFSRoots
    | (head args == "add-fs-root") && nonemptyTail args = doAddFsroot (args !! 1)
    | (head args == "remove-fs-root") && nonemptyTail args = doRmFsroot (args !! 1)
    |  otherwise = doUnparsable (head args)
    where
        nonemptyTail a = not . null $ tail a
        
main :: IO ()
main = do
    args <- getArgs
    doTheThing1 args

-- --------------------------------------------------------------------------------

doGeneralScan :: IO () 
doGeneralScan = SRM.generalScan


doGetMerkleRoots :: IO ()
doGetMerkleRoots = do
    scanroots <- FSM.getFSScanRoots
    mapM_ (\scanroot -> do
                putStrLn $ "location: " ++ scanroot
                merkle <- runMaybeT $ SRM.getCurrentMerkleRootForFSScanRoot scanroot 
                          -- IO ( Maybe String )
                putStrLn $ case merkle of
                    Nothing -> "merkle  : Unknown!"
                    Just m  -> "merkle  : " ++ m
            ) scanroots


doCasGet :: String -> IO () 
doCasGet key = do
        string <- CAS.get key
        putStrLn $ case string of
            Nothing -> "error: Could not retrive data for this key"
            Just s  ->  Char8.unpack s


doExposeAesonObject :: String -> IO ()
doExposeAesonObject aion_cas_hash = do
    aionJSONValueAsString <- CAS.get aion_cas_hash
    putStrLn $ 
        maybe "error: Could not retrive data for this key"
              (\a -> let aionJSONValue = Char8.unpack a in 
                if null aionJSONValue
                    then "error: I could not find a Content Addressable Store record"
                    else maybe "error: I could not convert the record to a Aeson Object" show 
                         (AOU.convertJSONStringIntoAesonValue aionJSONValue)
              ) aionJSONValueAsString


doRunQuery :: String -> IO ()
doRunQuery pattern = do
    scanroots <- FSM.getFSScanRoots
    mapM_ (\scanroot -> do
                putStrLn scanroot
                merkleroot <- runMaybeT $ SRM.getCurrentMerkleRootForFSScanRoot scanroot
                maybe (putStrLn "error: Could not retrieve Merkle root for this location")
                    (\mroot -> do
                        locationpaths <- runMaybeT $ SE.runQueryAgainMerkleRootUsingStoredData scanroot mroot pattern -- IO ( Maybe [ LocationPath ] )
                        maybe (putStrLn "error: Query has failed (for some reasons...)")
                            (mapM_ (\folderpath -> putStrLn $ "    " ++ folderpath ))
                            locationpaths)
                    merkleroot
            ) scanroots


doFsck :: IO ()
doFsck = do
    scanroots <- FSM.getFSScanRoots 
    mapM_ (\scanroot -> do
                putStrLn scanroot
                merkleroot' <- runMaybeT $ SRM.getCurrentMerkleRootForFSScanRoot scanroot
                case merkleroot' of
                    Nothing         -> putStrLn "error: Could not retrieve Merkle root for this location"
                    Just merkleroot -> do
                        bool <- SI.aionTreeFsckCASKey merkleroot
                        if bool
                            then putStrLn "Aion Tree is correct"
                            else putStrLn "error: Aion Tree does not check out"
           ) scanroots


doStatusReport :: IO ()
doStatusReport = do
    roots <- FSM.getFSScanRoots
    putStr "FS Scan Root file has "
    putStr $ show $ length roots
    putStrLn " elements"


doPrintFSRoots :: IO ()
doPrintFSRoots = FSM.printFSRootsListing


doAddFsroot :: String -> IO ()
doAddFsroot fsroot = FSM.addFSRoot fsroot


doRmFsroot :: String -> IO ()
doRmFsroot fsroot = FSM.removeFSRoot fsroot


doUnparsable :: String -> IO ()
doUnparsable token = do 
    putStrLn ""
    putStrLn $ "I could not interpret this: " ++ token 
    printHelp
    putStrLn ""


-- --------------------------------------------------------------------------------

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


