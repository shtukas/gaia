module Main where

{-
	runs at http://localhost:8000
	http://www.happstack.com
-}

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"

