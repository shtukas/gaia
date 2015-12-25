module Main where

import Control.Monad    (msum)
import Happstack.Server (dirs, path, nullConf, ok, simpleHTTP, serveDirectory, Browsing(EnableBrowsing), toResponse)

-- TODO: remove root directory hardcoding

main :: IO ()
main = simpleHTTP nullConf $
    msum [   dirs "api-v1" $ path ( \s -> ok $ toResponse $ "command: " ++ s )
           , dirs "static" $  ( serveDirectory EnableBrowsing ["index.html"] "/Lucille-E/Applications/Gaia/web-root" )
           , ok $ toResponse "404"
         ]


