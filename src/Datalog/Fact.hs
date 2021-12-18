module Datalog.Fact (Fact (..), getPredicate) where

import Datalog.Atom (Atom (predicate))
import Parseable (Parseable (parser))
import Pretty (Pretty (pretty))
import Prelude hiding (head)

newtype Fact = Fact {head :: Atom} deriving (Eq, Show, Ord)

instance Parseable Fact where
  parser = Fact <$> parser

instance Pretty Fact where
  pretty (Fact a) = pretty a

getPredicate :: Fact -> String
getPredicate = predicate . head
