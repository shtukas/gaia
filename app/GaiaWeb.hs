{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

runs at http://localhost:8000

http://www.happstack.com
http://www.happstack.com/page/view-page-slug/9/happstack-lite
This is my first use of happstack, I will be putting a lot of comments in the code.

HTML Templates : http://jaspervdj.be/blaze/tutorial.html

-}

module Main where

import           Control.Applicative         (optional, (<$>))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (unpack)
import           Happstack.Lite
import           Text.Blaze.Html5            (Html, a, form, input, label, p,
                                              toHtml, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size,
                                              type_, value)
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve Nothing myApp

-- Server runs at http://localhost:8000

myApp :: ServerPart Response
myApp = msum
   [
        dir "api" $ dir "ping" pong ,
        fileServing
   ]

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "web-root"

template :: Text -> Html -> Response
template title body = toResponse $
   H.html $ do
     H.head $ do
       H.title (toHtml title)
     H.body $ do
       body

pong :: ServerPart Response
pong =
    ok $ template "home page" $ do
            H.h1 "Pong"






