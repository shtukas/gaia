module Gaia.SystemIntegrity where

import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                as A
import qualified Gaia.AesonObjectsUtils    as GAOU

-- This function checks the Aion Tree below a CAS Key
aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- runMaybeT $ GAOU.getAesonJSONStringForCASKey caskey
    case string' of
        Nothing     -> return False
        Just string -> do
            let aesonValue = GAOU.convertJSONStringIntoAesonValue string
            maybe (return False) aionTreeFsckAesonValue aesonValue

-- This function checks the Aion Tree below a Aeson Value
aionTreeFsckAesonValue :: A.Value -> IO Bool
aionTreeFsckAesonValue aesonValue =
    if GAOU.aesonValueIsFile aesonValue
        then do
            let gaiaProjection = GAOU.aesonValueForFileGaiaProjection aesonValue
            aionTreeFsckFileGaiaProjection gaiaProjection
        else do
            let gaiaProjection = GAOU.aesonValueForDirectoryGaiaProjection aesonValue
            aionTreeFsckDirectoryGaiaProjection gaiaProjection

-- This function checks the Aion tree below a file trace
aionTreeFsckFileGaiaProjection :: ( String, Integer, String ) -> IO Bool -- ( filename, filesize, sha1-shah ) -> IO Bool
aionTreeFsckFileGaiaProjection gaiaProjection = return True

-- This function checks the Aion tree below
aionTreeFsckDirectoryGaiaProjection :: ( String, [String] ) -> IO Bool -- ( String, [String] ) -> IO Bool
aionTreeFsckDirectoryGaiaProjection gaiaProjection = do
    let caskeys = snd gaiaProjection
    bools <- mapM aionTreeFsckCASKey caskeys -- [ IO Bool ] ~mapM~> IO [Bool] ~> [Bool]
    return $ and bools

