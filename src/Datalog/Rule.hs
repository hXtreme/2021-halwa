module Datalog.Rule where

import Datalog.Atom
import Datalog.Literal
import PrettyPrinter

data Rule = Rule
  { head :: Atom,
    body :: [Literal]
  }
  deriving (Eq, Show)

instance Pretty Rule where
  prettyPrint r = (predicate (Datalog.Rule.head r)) ++ "(" ++ 
                  (prettyPrint (CommaSeparatedList (args (Datalog.Rule.head r)))) ++ 
                  ") :- " ++ (prettyPrint (CommaSeparatedList (body r)))
