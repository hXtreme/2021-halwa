module Datalog.Literal where

import Control.Applicative
import Datalog.Atom (Atom)
import Datalog.Constraint (Constraint)
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (Pretty (pretty))

data Literal
  = Pos Atom
  | Neg Atom
  | Constraint Constraint
  deriving (Eq, Show, Ord)

instance Parseable Literal where
  parser = negP <|> posP <|> constraintP
    where
      negP = Neg <$> (P.stringP "!" *> parser)
      posP = Pos <$> parser
      constraintP = Constraint <$> parser

instance Pretty Literal where
  pretty (Pos a) = pretty a
  pretty (Neg a) = "!" <> pretty a
  pretty (Constraint c) = pretty c
