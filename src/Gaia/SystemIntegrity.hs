module Gaia.SystemIntegrity where

import           Control.Monad.Trans.Maybe
import qualified Gaia.AesonObjectsUtils    as GAOU
import           Gaia.Types

-- This function checks the Aion Tree below a CAS Key
aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- runMaybeT $ GAOU.getAionJSONStringForCASKey caskey
    case string' of
        Nothing     -> return False
        Just string -> do
            let aesonValue = GAOU.convertJSONStringIntoAesonValue string
            aionTreeFsckAesonValue aesonValue
    where
         aionTreeFsckAesonValue Nothing = return False
         aionTreeFsckAesonValue (Just aesonValue) = do
                    let tap = GAOU.aesonValueToTAionPoint aesonValue
                    case tap of
                        TAionPointFile {}  -> return True
                        TAionPointDirectory _ contents -> do
                            list <- mapM aionTreeFsckCASKey contents
                            return $ and list
