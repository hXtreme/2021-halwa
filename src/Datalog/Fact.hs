module Datalog.Fact where

import Datalog.Atom
import PrettyPrinter
import Datalog.Argument

newtype Fact = Fact {head :: Atom} deriving (Eq, Show)

instance Pretty Fact where
  prettyPrint f = prettyPrint $ Datalog.Fact.head f
