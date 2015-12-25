{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad    (msum)
import Happstack.Server (dirs, path, nullConf, ok, simpleHTTP, serveDirectory, Browsing(EnableBrowsing), toResponse)
import Text.JSON.Generic

-- TODO: remove root directory hardcoding

alpha_search :: String -> [String]
alpha_search pattern = ["/Users/pascal/Desktop/alice.txt"]

main :: IO ()
main = simpleHTTP nullConf $
    msum [   dirs "api/v0" $ ok $ toResponse "Use the Force!"
           , dirs "api/v1/search" $ path ( \pattern -> ok $ toResponse $ encodeJSON $ alpha_search pattern )
           , serveDirectory EnableBrowsing ["index.html"] "/Lucille-E/Applications/Gaia/web-root"
         ]


