{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad    (msum)

import Happstack.Server (dirs, path, nullConf, simpleHTTP, 
                         serveDirectory, Browsing(EnableBrowsing),
                         ok,toResponse)

import Gaia.Types
import qualified Gaia.SearchEngine as GSE

import qualified Data.ByteString.Lazy.Char8 as Char8

alpha_search :: String -> SEStructure2
alpha_search pattern = SEStructure2 $ GSE.runQuery3 pattern

casKeyToDataWE :: String -> DataWE
casKeyToDataWE key = DataWE (Char8.pack "Use the Force!") "example.data"

main :: IO ()
main = simpleHTTP nullConf $
    msum [   dirs "api/v0" $ ok $ toResponse "Use the Force!"
           , dirs "api/v1/search" $ do path ( \pattern -> ok ( toResponse $ alpha_search pattern ) )
           , dirs "api/v1/aion-point" $ do path ( \s -> ok $ toResponse $ (casKeyToDataWE s) )
           , serveDirectory EnableBrowsing ["index.html"] "/Lucille-E/Applications/Gaia/web-root"
         ]

-- currently runs at http://localhost:8000
-- curl http://localhost:8000/api/v1/search/pascal
-- curl http://localhost:8000/api/v1/aion-point/a82e0c6ac0bf958fadbab48abc6345e0b6e83393
