module Datalog.Fact where

import Datalog.Atom (Atom)

newtype Fact = Fact {head :: Atom} deriving (Eq, Show)
