module Datalog.Program where

import Datalog.Declaration
import Datalog.Rule
import Datalog.Fact
import Datalog.Disjunction
import Datalog.Query
import Datalog.Constant
import Datalog.Argument
import Datalog.Variable
import Datalog.Atom
import Datalog.Literal
import Common
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
  prettyPrint (Program de r f di q) = prettyDeclarations ++ "\n\n" ++ prettyRules ++ 
                  "\n\n" ++ prettyFacts ++ "\n\n" ++ prettyDisjunctions ++
                  "\n\n" ++ prettyQueries
                  where
                    prettyDeclarations = prettyPrint (NewLineSeparatedList de)
                    prettyRules = prettyPrint (NewLineSeparatedList r)
                    prettyFacts = prettyPrint (NewLineSeparatedList f)
                    -- prettyDisjunctions = prettyPrint (NewLineSeparatedList di)
                    prettyDisjunctions = "Disjunction temp"
                    prettyQueries = prettyPrint (NewLineSeparatedList q)


test :: IO ()
test = let d1 = Declaration {Datalog.Declaration.predicate="hi", argTypes=[Common.Symbol, Common.Boolean]}
           d2 = Declaration {Datalog.Declaration.predicate="yo", argTypes=[Common.Integer, Common.String]}
           const1 = Datalog.Constant.Integer 3
           const2 = Datalog.Constant.Symbol "Symb"
           var1 = Datalog.Variable.Variable {name="var"}
           a1 = Atom {Datalog.Atom.predicate="test", args=[Constant const1, Datalog.Argument.Variable var1]}
           fact1 = Fact {Datalog.Fact.head=a1}
           lit1 = Pos a1
           rule1 = Rule {Datalog.Rule.head=a1, body=[lit1, lit1]}
           query1 = Query {query=a1}
           declarations = [d1, d2]
           rules = [rule1, rule1]
           facts = [fact1, fact1, fact1]
           queries = [query1, query1]
           disjunctions = []
           program1 = Program {declarations=declarations, rules=rules, Datalog.Program.facts=facts, queries=queries, disjunction=disjunctions}
        in putStrLn $ prettyPrint program1