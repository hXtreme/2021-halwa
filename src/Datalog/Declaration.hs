module Datalog.Declaration where

import Common (Type)
import Control.Applicative
import qualified Datalog.Common as DL.Common
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (CommaSeparatedList (CSL), Pretty (pretty))

data Declaration = Declaration
  { predicate :: String,
    argTypes :: [Type]
  }
  deriving (Eq, Show, Ord)

instance Parseable Declaration where
  parser = Declaration <$> (keyWordDecl *> name) <*> argTypes
    where
      keyWordDecl = P.stringP "decl"
      name = P.filter DL.Common.isNotKeyWord $ P.wsP P.snakeCaseWord
      argTypes = P.parensP (P.sepBy parser P.commaP) <|> pure []

instance Pretty Declaration where
  pretty (Declaration pred argTypes) =
    "decl " ++ pred
      ++ "("
      ++ pretty (CSL argTypes)
      ++ ")"
