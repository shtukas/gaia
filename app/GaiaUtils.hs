module Main where

import System.Environment
import qualified SearchEngine
import Data.Maybe
import System.Posix.User
import qualified ScanningAndRecordingManager
import qualified ContentAddressableStore
import qualified Data.Maybe as M

printHelp :: IO ()
printHelp = do
    putStrLn "    usage: gaia-utils run-scan-at-default-location"
    putStrLn "    usage: gaia-utils get-merkle-root"
    putStrLn "    usage: gaia-utils cas-get <key>"
    putStrLn "    usage: gaia-utils expose-aeon-object <key>"

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

    | ( (head args) == "expose-aeon-object" ) && ( length args >= 2 ) = do 
        let aion_cas_hash = ( head $ drop 1 args )
        aionJSONValueAsString <- ContentAddressableStore.get aion_cas_hash
        if ( length aionJSONValueAsString ) == 0
            then
                putStrLn "I could not find a ContentAddressableStore record"  
            else    
                do 
                    let aionJSONValueMaybe = SearchEngine.convertJSONStringIntoAeonJSONObject aionJSONValueAsString
                    if M.isJust aionJSONValueMaybe
                        then do 
                            let aionJSONValue = M.fromJust aionJSONValueMaybe
                            putStrLn $ show aionJSONValue
                        else
                            putStrLn "I could not convert the record to a Aeon Object"  

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

    Extracted using expose-aeon-object

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


