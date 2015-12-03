module Directives(
  DirectiveBody,
  DirectiveTag,
  Directive(..),
  parseDirectives,
  parseDirectivesFile
) where

import           Control.Monad          (fail)

import           Text.Parsec            (alphaNum, char, many, newline, parse,
                                         (<|>))
import           Text.Parsec.Char       (anyChar, string)
import           Text.Parsec.Combinator (many1, manyTill)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String     (Parser, parseFromFile)


-- NOTES
-- - to print UTF-8 character we cannot use putStr but instead we
--   will need to use Text or `mapM_ putChar`


-- SUPPORT TYPES
type DirectiveBody = String
data DirectiveTag  = Tag        -- | NewTag1 | NewTag2 ...
                     deriving (Eq, Show)

data Directive = Directive DirectiveTag DirectiveBody
                 deriving (Eq)

instance Show Directive where
  show (Directive t b) = show t ++ " -> " ++ "\"" ++ b ++ "\""


-- HELPERS
-- there is spaces in Parsec but it filters also newlines
spaces :: Parser String
spaces  = many (char ' ' <|> char '\t')

emptyLine :: Parser String
emptyLine  = do
               spaces
               newline
               return ""

restOfLine :: Parser String
restOfLine  = manyTill anyChar (char '\n')

comment :: Parser String
comment  = do
             char '#'
             spaces
             restOfLine
             return ""

ignorable :: Parser String
ignorable  = do
               many (comment <|> emptyLine)
               return ""

tag :: Parser DirectiveTag
tag  = (string "tag" >> return Tag)
       -- <|> (string "..." >> return ...) ...
       <|> (do
              t <- many (alphaNum <|> char '_')
              fail $ "Unknown tag: " ++ show t)

directive :: Parser Directive
directive  = do
               t <- tag
               char ':'
               spaces
               content <- restOfLine
               return (Directive t content)

directives :: Parser [Directive]
directives  = do
                string "gaia"
                d <- many1 (do
                              ignorable
                              directive)
                ignorable
                return d


-- DIRECTIVES PARSER
parseDirectives :: String -> Either ParseError [Directive]
parseDirectives  = parse directives ""

parseDirectivesFile :: String -> IO (Either ParseError [Directive])
parseDirectivesFile  = parseFromFile directives
