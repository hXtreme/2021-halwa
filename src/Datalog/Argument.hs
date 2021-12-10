module Datalog.Argument
  ( Argument (..),
    Constant,
    Variable,
  )
where

import Control.Applicative
import Datalog.Constant (Constant)
import Datalog.Variable (Variable)
import Parseable (Parseable, parser)
import Parser
import qualified Parser as P
import Pretty (Pretty (pretty))

data Argument
  = Wildcard
  | Constant Constant
  | Variable Variable
  deriving (Eq, Show, Ord)

instance Parseable Argument where
  parser = wildcardP <|> constantP <|> variableP
    where
      wildcardP = P.constP "_" Wildcard
      constantP = Constant <$> parser
      variableP = Variable <$> parser

instance Pretty Argument where
  pretty arg = case arg of
    Wildcard -> "_"
    Constant c -> pretty c
    Variable v -> pretty v
