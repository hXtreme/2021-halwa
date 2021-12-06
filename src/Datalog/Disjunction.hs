module Datalog.Disjunction where

import Datalog.Fact (Fact)
import Parseable (Parseable (parser))
import Control.Applicative
import qualified Parser as P

newtype Disjunction = Disjunction {facts :: [Fact]} deriving (Eq, Show)

instance Parseable Disjunction where
  parser = Disjunction <$> fewFacts
    where
        fewFacts = P.filter ((>= 2) . length) (P.sepBy parser P.semicolonP)
