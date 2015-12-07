module Gaia.SystemIntegrity where

import qualified Data.Aeson as A
import qualified Gaia.AesonObjectsUtils as GAOU
import           Gaia.Types

-- This function checks the Aion Tree below a CAS Key
aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- GAOU.getAionJSONStringForCASKey caskey
    case string' of 
        Nothing     -> return False
        Just string -> do
            let aesonValue = GAOU.convertJSONStringIntoAesonValue string
            case aesonValue of
                Nothing          -> return False
                Just aesonValue' -> do
                    let tap = GAOU.aesonValueToTAionPoint aesonValue'
                    case tap of 
                        TAionPointFile filename1 filesize1 hash1  -> return True
                        TAionPointDirectory foldername2 contents2 -> do
                            list <- sequence $ map ( \hash -> aionTreeFsckCASKey hash ) contents2
                            return $ and list