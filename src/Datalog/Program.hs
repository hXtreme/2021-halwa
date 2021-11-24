module Datalog.Program where

import Datalog.Declaration (Declaration)
import Datalog.Rule (Rule)
import Datalog.Fact (Fact)
import Datalog.Disjunction (Disjunction)
import Datalog.Query (Query)

data Program = Program
  { declarations :: [Declaration],
    rules :: [Rule],
    facts :: [Fact],
    disjunction :: [Disjunction],
    queries :: [Query]
  }
  deriving (Eq, Show)