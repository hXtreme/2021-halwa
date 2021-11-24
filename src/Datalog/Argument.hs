module Datalog.Argument where

import Datalog.Constant (Constant)
import Datalog.Variable (Variable)

data Argument
    =  Wildcard
    | Constant Constant
    | Variable Variable
    deriving (Eq, Show)