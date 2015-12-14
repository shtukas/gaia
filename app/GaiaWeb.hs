module Main where

{-
	http://www.happstack.com
	Runs at http://localhost:8000
-}

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"
