module Datalog.Program (Declaration, Atom, Rule, Fact, Disjunction, Query, Program) where

import Datalog.Atom (Atom)
import Datalog.Declaration (Declaration)
import Datalog.Disjunction (Disjunction)
import Datalog.Fact (Fact)
import Datalog.Query (Query)
import Datalog.Rule (Rule)

data Program = Program
  { declarations :: [Declaration],
    rules :: [Rule],
    facts :: [Fact],
    disjunction :: [Disjunction],
    queries :: [Query]
  }
  deriving (Eq, Show)