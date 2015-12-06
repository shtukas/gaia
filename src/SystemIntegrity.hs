module SystemIntegrity where

import qualified Data.Aeson as A
import qualified AesonObjectsUtils
import qualified Data.Maybe                 as M

-- This function checks the Aion Tree below a CAS Key
aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string <- AesonObjectsUtils.getAesonJSONStringForCASKey caskey
    let aesonValue = AesonObjectsUtils.convertJSONStringIntoAesonJSONObject $ string
    aionTreeFsckAesonValue $ M.fromJust aesonValue

-- This function checks the Aion Tree below a Aeson Value
aionTreeFsckAesonValue :: A.Value -> IO Bool
aionTreeFsckAesonValue aesonValue = do
    if AesonObjectsUtils.aesonValueIsFile aesonValue
        then do
            let gaiaProjection = AesonObjectsUtils.aesonValueForFileGaiaProjection aesonValue
            aionTreeFsckFileGaiaProjection gaiaProjection
        else do 
            let gaiaProjection = AesonObjectsUtils.aesonValueForDirectoryGaiaProjection aesonValue
            aionTreeFsckDirectoryGaiaProjection gaiaProjection

-- This function checks the Aion tree below a file trace
aionTreeFsckFileGaiaProjection :: ( String, Integer, String ) -> IO Bool -- ( filename, filesize, sha1-shah ) -> IO Bool
aionTreeFsckFileGaiaProjection gaiaProjection = do
    return True

-- This function checks the Aion tree below 
aionTreeFsckDirectoryGaiaProjection :: ( String, [String] ) -> IO Bool -- ( String, [String] ) -> IO Bool
aionTreeFsckDirectoryGaiaProjection gaiaProjection = do
    let caskeys = snd gaiaProjection
    let bools1 = map (\key -> aionTreeFsckCASKey key ) caskeys -- [ IO Bool ]
    let bools2 = sequence bools1 -- IO [ Bool ]
    bools3 <- bools2 -- [ Bool ]
    return $ all (\b -> b) bools3 -- Funny that it has to be done that way





