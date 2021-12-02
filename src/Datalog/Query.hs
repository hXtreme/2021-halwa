module Datalog.Query where

import Datalog.Atom
import PrettyPrinter

newtype Query = Query {query :: Atom} deriving (Eq, Show)

instance Pretty Query where
  prettyPrint q = "output " ++ predicate (query q)