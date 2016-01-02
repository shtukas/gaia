module Gaia.SystemIntegrity where

import qualified Data.Aeson as A
import qualified Gaia.AionPointAbstractionUtils as GAOU
import           Gaia.Types

aionTreeFsckAesonValue :: A.Value -> IO Bool
aionTreeFsckAesonValue aesonValue = do
    let tap = GAOU.aesonValueToAionPointAbstractionGeneric aesonValue
    case tap of
        AionPointAbstractionGenericFromFile _ -> return True
        AionPointAbstractionGenericFromDirectory (AionPointAbstractionDirectory _ contents) -> do
            -- map aionTreeFsckCASKey contents :: [IO Bool]
            -- sequence $ map aionTreeFsckCASKey contents :: IO [Bool]
            list <- sequence $ map aionTreeFsckCASKey contents
            -- list :: [Bool]
            return $ and list

-- This function checks the Aion Tree below a CAS Key
aionTreeFsckCASKey :: String -> IO Bool
aionTreeFsckCASKey caskey = do
    string' <- GAOU.getAionJSONStringForCASKey3 caskey
    case string' of
        Nothing     -> return False
        Just string -> do
            -- convertJSONStringIntoAesonValue :: String -> Maybe A.Value
            case (GAOU.convertJSONStringIntoAesonValue string) of
                Nothing -> return False
                Just avalue -> aionTreeFsckAesonValue avalue

