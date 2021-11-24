module Datalog.Rule where

import Datalog.Atom (Atom)
import Datalog.Literal (Literal)

data Rule = Rule
  { head :: Atom,
    body :: [Literal]
  }
  deriving (Eq, Show)
