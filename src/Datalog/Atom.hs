module Datalog.Atom where

import Control.Applicative
import Datalog.Argument (Argument)
import qualified Datalog.Common as DL.Common
import Parseable (Parseable (parser))
import qualified Parser as P

data Atom = Atom
  { predicate :: String,
    args :: [Argument]
  }
  deriving (Eq, Show)

instance Parseable Atom where
  parser = Atom <$> predicate <*> args
    where
      predicate = P.filter DL.Common.isNotKeyWord $ P.wsP P.snakeCaseWord
      args = P.parensP (P.sepBy parser P.commaP) <|> pure []
