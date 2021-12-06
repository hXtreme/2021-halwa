module Datalog.Argument(
  Argument(..),
  Constant,
  Variable,
) where

import Control.Applicative
import Datalog.Constant (Constant)
import Datalog.Variable (Variable)
import Parseable (Parseable, parser)
import Parser
import qualified Parser as P

data Argument
  = Wildcard
  | Constant Constant
  | Variable Variable
  deriving (Eq, Show)

instance Parseable Argument where
  parser = wildcardP <|> constantP <|> variableP
    where
      wildcardP = P.constP "_" Wildcard
      constantP = Constant <$> parser
      variableP = Variable <$> parser