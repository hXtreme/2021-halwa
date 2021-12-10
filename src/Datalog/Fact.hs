module Datalog.Fact where

import Datalog.Atom (Atom)
import Parseable (Parseable (parser))
import Pretty (Pretty (pretty))

newtype Fact = Fact {head :: Atom} deriving (Eq, Show, Ord)

instance Parseable Fact where
  parser = Fact <$> parser

instance Pretty Fact where
  pretty (Fact a) = pretty a
