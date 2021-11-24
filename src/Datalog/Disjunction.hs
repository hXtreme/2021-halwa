module Datalog.Disjunction where

import Datalog.Fact (Fact)

newtype Disjunction = Disjunction {facts :: [Fact]} deriving (Eq, Show)