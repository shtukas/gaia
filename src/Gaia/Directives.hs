module Gaia.Directives(
  DirectiveBody,
  DirectiveTag,
  Directive(..),
  parseDirectives,
  parseDirectivesFile
) where

import           Gaia.Types

import           Text.Parsec            (alphaNum, char, many, newline, parse,
                                         try, (<|>))
import           Text.Parsec.Char       (anyChar, string)
import           Text.Parsec.Combinator (many1, manyTill)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String     (Parser, parseFromFile)

type Filepath = String

-- NOTES
-- - to print UTF-8 character we cannot use putStr but instead we
--   will need to use Text or `mapM_ putChar`


-- HELPERS
-- note: we redefine spaces because Text.Parsec.spaces filters also newlines
spaces :: Parser String
spaces  = many (char ' ' <|> char '\t') *> return ""

emptyLine :: Parser String
emptyLine  = spaces *> newline *> return ""

restOfLine :: Parser String
restOfLine  = manyTill anyChar (char '\n')

comment :: Parser String
comment  = char '#' *> spaces *> restOfLine

ignorable :: Parser String
ignorable  = many (comment <|> emptyLine) *> return ""

tag :: Parser DirectiveTag
tag  = try (string "tag" >> return Tag)
       -- <|> (string "..." >> return ...) ...
       <|> (do
              t <- many (alphaNum <|> char '_')
              fail $ "Unknown tag: " ++ show t)

directive :: Parser Directive
directive  = do
               t <- tag
               _ <- char ':' *> spaces
               content <- restOfLine
               return (Directive t content)

directives :: Parser [Directive]
directives  = do
                _ <- string "gaia"
                d <- many1 $ try (ignorable >> directive)
                _ <- ignorable
                return d


-- DIRECTIVES PARSER
parseDirectives :: String -> Either ParseError [Directive]
parseDirectives  = parse directives ""

parseDirectivesFile :: Filepath -> IO (Either ParseError [Directive])
parseDirectivesFile  = parseFromFile directives
