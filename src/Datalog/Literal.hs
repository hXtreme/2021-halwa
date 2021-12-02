module Datalog.Literal where

import Datalog.Atom (Atom)
import Datalog.Constraint (Constraint)
import PrettyPrinter

data Literal
  = Pos Atom
  | Neg Atom
  | Constraint Constraint
  deriving (Eq, Show)

instance Pretty Literal where
      -- Change "test" as it's a temporary approach
  prettyPrint l = "test"  
