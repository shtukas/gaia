module Gaia.SystemIntegrity (
    aionTreeFsckCASKey
) where

import qualified Gaia.AesonValuesFileSystemCorrespondance as XP1
import qualified Gaia.AesonValuesAionPointAbstractionsCorrespondance as XP2
import           Gaia.Types

aionTreeFsckAesonValue :: ExtendedAesonValue -> IO Bool
aionTreeFsckAesonValue (ExtendedAesonValue aesonValue _) = do
    let (ExtendedAionPointAbstractionGeneric aionPointAbstractionGeneric caskey) = XP2.extendedAesonValueToExtendedAionPointAbstractionGeneric (ExtendedAesonValue aesonValue caskey)
    case aionPointAbstractionGeneric of
        AionPointAbstractionGenericFromFile _ -> return True
        AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory _ contents) -> do
            list <- sequence $ map aionTreeFsckCASKey contents
            return $ and list

aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- XP1.getAionJSONStringForCASKey3 caskey
    case string' of
        Nothing     -> return False
        Just string -> do
            case (XP1.convertJSONStringIntoAesonValue string) of
                Nothing -> return False
                Just avalue -> aionTreeFsckAesonValue (ExtendedAesonValue avalue caskey)

