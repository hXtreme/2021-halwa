module Datalog.Literal where

import Datalog.Atom
import Datalog.Constraint
import PrettyPrinter

data Literal
  = Pos Atom
  | Neg Atom
  | Constraint Constraint
  deriving (Eq, Show)

instance Pretty Literal where
      -- Change "temp" as it's a temporary approach
  prettyPrint (Pos a) = (predicate a) ++ "(" ++ (prettyPrint (CommaSeparatedList (args a))) ++ ")"
  prettyPrint (Neg a) = (predicate a) ++ "(" ++ (prettyPrint (CommaSeparatedList (args a))) ++ ")"
  prettyPrint (Constraint c) = prettyPrint c