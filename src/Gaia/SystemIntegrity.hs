module Gaia.SystemIntegrity (
    aionTreeFsckCASKey
) where

import qualified Gaia.AionPointAbstractionUtils as GAOU
import           Gaia.Types

aionTreeFsckAesonValue :: ExtendedAesonValue -> IO Bool
aionTreeFsckAesonValue (ExtendedAesonValue aesonValue _) = do
    let (ExtendedAionPointAbstractionGeneric aionPointAbstractionGeneric caskey) = GAOU.extendedAesonValueToExtendedAionPointAbstractionGeneric (ExtendedAesonValue aesonValue caskey)
    case aionPointAbstractionGeneric of
        AionPointAbstractionGenericFromFile _ -> return True
        AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory _ contents) -> do
            list <- sequence $ map aionTreeFsckCASKey contents
            return $ and list

aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- GAOU.getAionJSONStringForCASKey3 caskey
    case string' of
        Nothing     -> return False
        Just string -> do
            -- convertJSONStringIntoAesonValue :: String -> Maybe A.Value
            case (GAOU.convertJSONStringIntoAesonValue string) of
                Nothing -> return False
                Just avalue -> aionTreeFsckAesonValue (ExtendedAesonValue avalue caskey)

