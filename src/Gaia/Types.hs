module Gaia.Types where

-- -----------------------------------------------------------------------------
-- General File System

type FolderPath   = String
type LocationPath = String

-- -----------------------------------------------------------------------------
-- Aion Points

-- See http://stackoverflow.com/questions/24352280/multiple-declarations-of-x
-- for why I use "unnatural" field names

data TAionPoint = TAionPointFile { name1 :: String
                                 , size1 :: Integer
                                 , hash1 :: String }
                | TAionPointDirectory  { name2     :: String
                                       , contents2 :: [String] }
    deriving (Show)

-- -----------------------------------------------------------------------------
-- Gaia Files

type GaiaFileDirectiveBody = String
data GaiaFileDirectiveTag  = GaiaFileTag        -- | NewTag1 | NewTag2 ...
                     deriving (Eq, Show)

data GaiaFileDirective = GaiaFileDirective GaiaFileDirectiveTag GaiaFileDirectiveBody
                 deriving (Eq)

instance Show GaiaFileDirective where
  show (GaiaFileDirective t b) = show t ++ " -> " ++ "\"" ++ b ++ "\""
