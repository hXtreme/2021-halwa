module Datalog.Literal where

import Control.Applicative
import Datalog.Atom (Atom)
import Datalog.Constraint (Constraint)
import Parseable (Parseable (parser))
import qualified Parser as P

data Literal
  = Pos Atom
  | Neg Atom
  | Constraint Constraint
  deriving (Eq, Show)

instance Parseable Literal where
  parser = negP <|> posP <|> constraintP
    where
      negP = Neg <$> (P.stringP "!" *> parser)
      posP = Pos <$> parser
      constraintP = Constraint <$> parser
