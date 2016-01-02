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
-- Aion Points, AionPointAbstractionGeneric
{-
	AionPointAbstractionGeneric are "projections" of Aeson Values.
	They are used to make the code of the search engine independent from Aeson. 
-}

-- See http://stackoverflow.com/questions/24352280/multiple-declarations-of-x
-- for why I use "unnatural" field names.

data AionPointAbstractionFile = AionPointAbstractionFile { name1 :: String
                                     , size1 :: Integer
                                     , hash1 :: String } deriving (Show)

data AionPointAbstractionDirectory = AionPointAbstractionDirectory { name2 :: String
                                               , contents2 :: [String] } deriving (Show)

data AionPointAbstractionGeneric = AionPointAbstractionGenericFromFile AionPointAbstractionFile | AionPointAbstractionGenericFromDirectory AionPointAbstractionDirectory 
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
{-
	SEStructure1 encapsulates answers from the Search Engine. 
	It is used because we needed to make them instances of Happstack's ToMessage classtype.
-}

newtype SEStructure1 = SEStructure1C [String]

instance R.ToMessage SEStructure1 where
  toContentType _ = B1.pack "application/json"
  toMessage (SEStructure1C x) = (Char8.pack . TJG.encodeJSON) x


