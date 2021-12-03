module Datalog.Atom where

import Datalog.Argument (Argument)
import PrettyPrinter

data Atom = Atom
  { predicate :: String,
    args :: [Argument]
  }
  deriving (Eq, Show)

instance Pretty Atom where
  prettyPrint a = (predicate a) ++ "(" ++ (prettyPrint (CommaSeparatedList (args a))) ++ ")"
