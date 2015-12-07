module Gaia.Directives(
  GaiaFileDirectiveBody,
  GaiaFileDirectiveTag,
  GaiaFileDirective(..),
  parseDirectives,
  parseDirectivesFile,
  checkDirectivesFile
) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Gaia.Types
import           Text.Parsec            (alphaNum, char, many, newline, parse,
                                         try, (<|>))
--  parse :: Parser a -> SourceName -> String -> Either ParseError a
--  I leave it here in case we decide to go back to Error messages and EitherT
import           Text.Parsec.Char       (anyChar, string)
import           Text.Parsec.Combinator (many1, manyTill)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String     (Parser, parseFromFile)

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

tag :: Parser GaiaFileDirectiveTag
tag  = try (string "tag" >> return GaiaFileTag)
       -- <|> (string "..." >> return ...) ...
       <|> (do
              t <- many (alphaNum <|> char '_')
              fail $ "Unknown tag: " ++ show t)

directive :: Parser GaiaFileDirective
directive  = do
               t <- tag
               _ <- char ':' *> spaces
               content <- restOfLine
               return (GaiaFileDirective t content)

directives :: Parser [GaiaFileDirective]
directives  = do
                _ <- string "gaia"
                d <- many1 $ try (ignorable >> directive)
                _ <- ignorable
                return d

-- ERROR MANAGEMENT
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

hsuh :: Either a b -> Maybe a
hsuh = either Just (const Nothing)

-- DIRECTIVES PARSER
parseDirectives :: String -> Maybe [GaiaFileDirective]
parseDirectives str = hush (parse directives "" str)

parseDirectivesFile :: FilePath -> MaybeT IO [GaiaFileDirective]
parseDirectivesFile filepath = do
    directs <- MaybeT $ liftM hush eitherDirectives
    return directs
    where
        eitherDirectives = parseFromFile directives filepath

checkDirectivesFile :: FilePath -> MaybeT IO ParseError
checkDirectivesFile filepath = do
  errors <- MaybeT $ liftM hsuh eitherDirectives
  return errors 
  where 
    eitherDirectives = (parseFromFile directives filepath)
