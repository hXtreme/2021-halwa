module Datalog.Program where

import Datalog.Declaration (Declaration)
import Datalog.Rule (Rule)
import Datalog.Fact (Fact)
import Datalog.Disjunction (Disjunction)
import Datalog.Query (Query)
import PrettyPrinter

-- https://souffle-lang.github.io/examples

data Program = Program
  { declarations :: [Declaration],
    rules :: [Rule],
    -- Rule example: alias(X,X) :- assign(X,_).
    facts :: [Fact],
    disjunction :: [Disjunction],
    queries :: [Query]
    -- Query example: .output pointsTo
  }
  deriving (Eq, Show)

instance Pretty Program where
  prettyPrint (Program de r f di q) = prettyDeclarations ++ "\n" ++ prettyRules ++ 
                  "\n" ++ prettyFacts ++ "\n" ++ prettyDisjunctions ++
                  "\n" ++ prettyQueries
                  where
                    prettyDeclarations = prettyPrint (NewLineSeparatedList de)
                    prettyRules = prettyPrint (NewLineSeparatedList r)
                    prettyFacts = prettyPrint (NewLineSeparatedList f)
                    -- prettyDisjunctions = prettyPrint (NewLineSeparatedList di)
                    prettyDisjunctions = "Disjunction temp"
                    prettyQueries = prettyPrint (NewLineSeparatedList q)