module Datalog.Fact where

import Datalog.Atom
import PrettyPrinter

newtype Fact = Fact {head :: Atom} deriving (Eq, Show)

instance Pretty Fact where
  prettyPrint f = (predicate (Datalog.Fact.head f)) ++ "(" ++ (prettyPrint (CommaSeparatedList (args (Datalog.Fact.head f)))) ++ ")"
