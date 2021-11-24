module Datalog.Atom where

import Datalog.Argument (Argument)

data Atom = Atom
  { predicate :: String,
    args :: [Argument]
  }
  deriving (Eq, Show)
