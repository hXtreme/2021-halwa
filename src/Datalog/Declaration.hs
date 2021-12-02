module Datalog.Declaration where

import Common (Type)
import PrettyPrinter

data Declaration = Declaration
  { predicate :: String,
    argTypes :: [Type]
  }
  deriving (Eq, Show)


instance Pretty Declaration where
  prettyPrint d = "decl " ++ (predicate d) ++ "(" ++ (prettyPrint (CommaSeparatedList (argTypes d))) ++ ")"

