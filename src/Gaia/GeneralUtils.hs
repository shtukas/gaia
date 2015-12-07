module Gaia.GeneralUtils where

import qualified Data.Char as Char

-- Stuff that should exist but somehow do not.

stringToLower :: String -> String
stringToLower input = map Char.toLower input
