module Datalog.Constraint where

import Common (BinOP, UnOP (Pos))
import Control.Applicative
import Datalog.Argument (Argument)
import Parseable (Parseable (parser))
import Pretty (Pretty (pretty))

data Constraint
  = BinaryConstraint Argument BinOP Argument
  | UnaryConstraint UnOP Argument
  deriving (Eq, Show)

instance Parseable Constraint where
  parser =
    BinaryConstraint <$> parser <*> parser <*> parser
      <|> UnaryConstraint <$> parser <*> parser

instance Pretty Constraint where
  pretty (BinaryConstraint a op b) =
    "(" ++ pretty a ++ " " ++ pretty op ++ " " ++ pretty b <> ")"
  pretty (UnaryConstraint op a) = case op of
    Pos -> pretty a
    _ -> "(" ++ pretty op <> pretty a ++ ")"
