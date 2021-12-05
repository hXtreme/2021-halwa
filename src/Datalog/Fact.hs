module Datalog.Fact where

import Datalog.Atom (Atom)
import Parseable (Parseable (parser))

newtype Fact = Fact {head :: Atom} deriving (Eq, Show)

instance Parseable Fact where
  parser = Fact <$> parser
