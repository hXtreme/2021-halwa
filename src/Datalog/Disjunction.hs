module Datalog.Disjunction where

import Control.Applicative
import Data.List (intercalate)
import Datalog.Fact (Fact)
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (Pretty (pretty))

newtype Disjunction = Disjunction {facts :: [Fact]} deriving (Eq, Show)

instance Parseable Disjunction where
  parser = Disjunction <$> fewFacts
    where
      fewFacts = P.filter ((>= 2) . length) (P.sepBy parser P.semicolonP)

instance Pretty Disjunction where
  pretty (Disjunction fs) = intercalate "; " (map pretty fs)
