module Gaia.Types where

type DirectiveBody = String
data DirectiveTag  = Tag        -- | NewTag1 | NewTag2 ...
                     deriving (Eq, Show)

data Directive = Directive DirectiveTag DirectiveBody
                 deriving (Eq)

instance Show Directive where
  show (Directive t b) = show t ++ " -> " ++ "\"" ++ b ++ "\""
