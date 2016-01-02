module Main where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Gaia.AionPointAbstractionUtils as AOU
import qualified Gaia.FSRootsManagement as FSM
import qualified Gaia.ScanningAndRecordingManager as SRM
import qualified Gaia.SearchEngine as SE
import qualified Gaia.SystemIntegrity as SI
import qualified PStorageServices.ContentAddressableStore as CAS
import           System.Environment

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

    | null args = do
        putStrLn ""
        printHelp
        putStrLn ""

    | head args == "general-scan" = do
        SRM.generalScan

    | head args == "get-merkle-roots" = do
        scanroots <- FSM.getFSScanRoots
        _ <- mapM ( \scanroot -> do
                        putStrLn $ "location: "++scanroot
                        merkle <- SRM.getCurrentMerkleRootForFSScanRoot scanroot -- IO ( Maybe String )
                        case merkle of
                            Nothing      -> putStrLn "merkle  : Unknown!"
                            Just merkle' -> putStrLn $ "merkle  : " ++ merkle'
                    ) scanroots
        return ()

    | ( head args == "cas-get" ) && ( length args >= 2 ) = do
        let key = args !! 1
        string <- CAS.get key
        case string of
            Nothing      -> putStrLn "error: Could not retrive data for this key"
            Just string' -> putStrLn $ Char8.unpack string'

    | ( head args == "expose-aeson-object" ) && ( length args >= 2 ) = do
        let aion_cas_hash = args !! 1
        aionJSONValueAsString <- CAS.get aion_cas_hash
        case aionJSONValueAsString of
            Nothing                     -> putStrLn "error: Could not retrive data for this key"
            Just aionJSONValueAsString' ->
                if ( length $ Char8.unpack aionJSONValueAsString' ) == 0
                    then putStrLn "I could not find a Content Addressable Store record"
                    else do
                        let aionJSONValueMaybe = AOU.convertJSONStringIntoAesonValue ( Char8.unpack aionJSONValueAsString' )
                        case aionJSONValueMaybe of
                            Nothing            -> putStrLn "I could not convert the record to a Aeson Object"
                            Just aionJSONValue -> print aionJSONValue

    | ( head args == "run-query" ) && ( length args >= 2 ) = do
        let pattern = args !! 1
        let seatoms = SE.runQuery2 pattern -- runQuery2 :: String -> [SEAtom]
        _ <- sequence $ map ( \seatom -> putStrLn $ show seatom ) seatoms
        return ()

    | head args == "fsck" = do
        scanroots <- FSM.getFSScanRoots
        _ <- mapM ( \scanroot -> do
                        putStrLn scanroot
                        merkleroot' <- SRM.getCurrentMerkleRootForFSScanRoot scanroot
                        case merkleroot' of
                            Nothing         -> putStrLn "error: Could not retrieve Merkle root for this location"
                            Just merkleroot -> do
                                bool <- SI.aionTreeFsckCASKey merkleroot
                                if bool
                                    then putStrLn "Aion Tree is correct"
                                    else putStrLn "error: Aion Tree does not check out"

                    ) scanroots
        return ()

    | head args == "status-report" = do
        roots <- FSM.getFSScanRoots
        putStr "FS Scan Root file has "
        putStr $ show $ length roots
        putStrLn " elements"

    | head args == "print-fs-roots" = do
        FSM.printFSRootsListing

    | ( head args == "add-fs-root" ) && ( length args >= 2 ) = do
        let fsroot = args !! 1
        FSM.addFSRoot fsroot

    | ( head args == "remove-fs-root" ) && ( length args >= 2 ) = do
        let fsroot = args !! 1
        FSM.removeFSRoot fsroot

    | otherwise = do
        putStrLn ""
        putStrLn $ "I could not interpret this: " ++ head args
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


