module Gaia.Types where

import Happstack.Server.Response as R
import qualified Data.ByteString.Char8 as B1
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Text.JSON.Generic as TJG

-- -----------------------------------------------------------------------------
-- General File System

type FolderPath   = String
type LocationPath = String

-- -----------------------------------------------------------------------------
-- Aion Points

-- See http://stackoverflow.com/questions/24352280/multiple-declarations-of-x
-- for why I use "unnatural" field names.

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

-- -----------------------------------------------------------------------------
-- FileSystemSearchEngine

data SEStructure1 = SEStructure1C [String]

instance R.ToMessage SEStructure1 where
  toContentType _ = B1.pack "application/json"
  toMessage (SEStructure1C x) = (Char8.pack . TJG.encodeJSON) x


