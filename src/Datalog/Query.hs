module Datalog.Query where

import Datalog.Atom
import PrettyPrinter

newtype Query = Query {query :: Atom} deriving (Eq, Show)

instance Pretty Query where
  prettyPrint f = "output " ++ predicate (query f)