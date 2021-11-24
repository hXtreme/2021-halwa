module Datalog.Literal where

import Datalog.Atom (Atom)
import Datalog.Constraint (Constraint)

data Literal
  = Pos Atom
  | Neg Atom
  | Constraint Constraint
  deriving (Eq, Show)
