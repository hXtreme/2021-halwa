module Common (Type) where

data Type
  = Symbol
  | Integer
  | Boolean
  | String String
  deriving (Eq, Show)