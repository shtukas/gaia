{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad    (msum)

import Happstack.Server (dirs, path, nullConf, simpleHTTP, 
                         serveDirectory, Browsing(EnableBrowsing))
import Happstack.Server.Response as R

import Text.JSON.Generic
import qualified Data.ByteString.Char8 as B1
import qualified Data.ByteString.Lazy.Char8 as Char8

data Patterns = PatternsC [String]

instance R.ToMessage Patterns where
  toContentType _ = B1.pack "application/json"
  toMessage (PatternsC x) = (Char8.pack . encodeJSON) x

alpha_search :: String -> Patterns
alpha_search pattern = PatternsC ["/Users/pascal/Desktop/alice.txt"]

main :: IO ()
main = simpleHTTP nullConf $
    msum [   dirs "api/v0" $ ok $ toResponse "Use the Force!"
           , dirs "api/v1/search" $ do path ( \pattern -> ok ( toResponse $ alpha_search pattern ) )
           , serveDirectory EnableBrowsing ["index.html"] "/Lucille-E/Applications/Gaia/web-root"
         ]

-- currently runs at http://localhost:8000
-- TODO: remove root directory hardcoding

