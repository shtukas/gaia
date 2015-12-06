{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- http://www.happstack.com
-- http://localhost:8000/echo

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
   [ 
     -- dir "hello"   $ hello
     -- , 
     homePage
   ]

template :: Text -> Html -> Response
template title body = toResponse $
   H.html $ do
     H.head $ do
       H.title (toHtml title)
     H.body $ do
       body
       p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
            H.h1 "Hello!"
            H.p "Writing applications with happstack-lite is fast and simple!"
            H.p "Check out these killer apps."
            H.p $ a ! href "/echo/secret%20message"  $ "echo"
            H.p $ a ! href "/query?foo=bar" $ "query parameters"
            H.p $ a ! href "/form"          $ "form processing"
            H.p $ a ! href "/fortune"       $ "(fortune) cookies"
            H.p $ a ! href "/files"         $ "file serving"
            H.p $ a ! href "/upload"        $ "file uploads"


