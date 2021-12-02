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

instance Pretty Program where
  prettyPrint (de, r, f, di, q) = prettyDeclarations ++ "\n" ++ prettyRules ++ 
                  "\n" ++ prettyFacts ++ "\n" ++ prettyDisjunctions ++
                  "\n" ++ prettyQueries
                  where
                    prettyDeclarations = prettyPrint (NewLineSeparatedList de)
                    prettyRules = prettyPrint (NewLineSeparatedList r)
                    prettyFacts = prettyPrint (NewLineSeparatedList f)
                    prettyDisjunctions = prettyPrint (NewLineSeparatedList di)
                    prettyQueries = prettyPrint (NewLineSeparatedList q)