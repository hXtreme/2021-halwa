module Datalog.Declaration where

import Common (Type)

data Declaration = Declaration
  { predicate :: String,
    argTypes :: [Type]
  }
  deriving (Eq, Show)
