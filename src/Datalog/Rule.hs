module Datalog.Rule where

import Datalog.Atom (Atom)
import Datalog.Literal (Literal)
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (CommaSeparatedList (CSL), Pretty (pretty))

data Rule = Rule
  { head :: Atom,
    body :: [Literal]
  }
  deriving (Eq, Show)

instance Parseable Rule where
  parser = Rule <$> parser <*> (P.stringP ":-" *> bodyP)
    where
      bodyP = P.sepBy1 parser P.commaP

instance Pretty Rule where
  pretty (Rule head body) = pretty head <> " :- " <> pretty (CSL body)
