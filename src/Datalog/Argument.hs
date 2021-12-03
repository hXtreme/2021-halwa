module Datalog.Argument where

import Datalog.Constant
import Datalog.Variable
import PrettyPrinter

data Argument
    =  Wildcard
    | Constant Constant
    | Variable Variable
    deriving (Eq, Show)


instance Pretty Argument where
  prettyPrint (Constant c) = show c
  prettyPrint (Datalog.Argument.Variable v) = name v
  prettyPrint _ = "_"