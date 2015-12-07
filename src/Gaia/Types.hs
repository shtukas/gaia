module Gaia.Types where

-- Genreral File System

type FolderPath   = String
type LocationPath = String

-- Gaia Files

type GaiaFileDirectiveBody = String
data GaiaFileDirectiveTag  = GaiaFileTag        -- | NewTag1 | NewTag2 ...
                     deriving (Eq, Show)

data GaiaFileDirective = GaiaFileDirective GaiaFileDirectiveTag GaiaFileDirectiveBody
                 deriving (Eq)

instance Show GaiaFileDirective where
  show (GaiaFileDirective t b) = show t ++ " -> " ++ "\"" ++ b ++ "\""
