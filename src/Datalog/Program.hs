module Datalog.Program
  ( Program (..),
    Item (..),
  )
where

import Control.Applicative
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Datalog.Atom (Atom)
import qualified Datalog.Common as DL.Common
import Datalog.Declaration (Declaration)
import Datalog.Disjunction (Disjunction)
import Datalog.Fact (Fact)
import Datalog.Query (Query)
import Datalog.Rule (Rule)
import Parseable (Parseable (parser))
import Parser (Parser)
import qualified Parser as P
import Pretty (NewLineSeparatedList (NSL), Pretty (pretty))

data Program = Program
  { declarations :: [Declaration],
    rules :: [Rule],
    facts :: [Fact],
    disjunction :: [Disjunction],
    queries :: [Query]
  }
  deriving (Eq, Show)

instance Parseable Program where
  parser = do
    items <- many parser <* P.eof
    let decls = declarationsFilter items
    let rules = rulesFilter items
    let facts = factsFilter items
    let disjs = disjunctionFilter items
    let queries = queriesFilter items
    return $ Program decls rules facts disjs queries

instance Pretty Program where
  pretty (Program decls rules facts disjs queries) =
    intercalate
      "\n\n"
      [ fmt decls,
        fmt facts,
        fmt rules,
        fmt disjs,
        fmt queries
      ]
    where
      terminated x = pretty x <> DL.Common.end
      fmt its = pretty . NSL $ map terminated its

-- >>> P.parse ((parser :: Parser Program)) "p(X, Y) :- q(X, Y), r(X)."
-- Right (L (Location 1 1) (Program {declarations = [], rules = [Rule {head = Atom {predicate = "p", args = [Variable (Variable {name = "X"}),Variable (Variable {name = "Y"})]}, body = [Pos (Atom {predicate = "q", args = [Variable (Variable {name = "X"}),Variable (Variable {name = "Y"})]}),Pos (Atom {predicate = "r", args = [Variable (Variable {name = "X"})]})]}], facts = [], disjunction = [], queries = []}))

-- >>> P.parse (((parser :: Parser Item)) <* P.eof) "p(X) :- q(X), r(X)."
-- Right (L (Location 1 1) (Rule (Rule {head = Atom {predicate = "p", args = [Variable (Variable {name = "X"})]}, body = [Pos (Atom {predicate = "q", args = [Variable (Variable {name = "X"})]}),Pos (Atom {predicate = "r", args = [Variable (Variable {name = "X"})]})]})))

-- >>> P.parse ((many (parser :: Parser Item)) <* P.eof) "p(X) :- q(X), r(X)."
-- Right (L (Location 1 1) [Rule (Rule {head = Atom {predicate = "p", args = [Variable (Variable {name = "X"})]}, body = [Pos (Atom {predicate = "q", args = [Variable (Variable {name = "X"})]}),Pos (Atom {predicate = "r", args = [Variable (Variable {name = "X"})]})]})])

data Item
  = Comment String
  | Decl Declaration
  | Fact Fact
  | Disj Disjunction
  | Rule Rule
  | Query Query
  deriving (Eq, Show)

instance Parseable Item where
  parser =
    commentP
      <|> declItemP
      <|> factItemP
      <|> disjItemP
      <|> ruleItemP
      <|> queryItemP
    where
      commentP = Comment <$> (P.stringP "//" *> P.wsP P.line)
      declItemP = Decl <$> parser <* itemEndP
      factItemP = Fact <$> parser <* itemEndP
      disjItemP = Disj <$> parser <* itemEndP
      ruleItemP = Rule <$> parser <* itemEndP
      queryItemP = Query <$> parser <* itemEndP
      itemEndP = P.stringP DL.Common.end

declarationsFilter :: [Item] -> [Declaration]
declarationsFilter =
  mapMaybe
    ( \it -> case it of
        Decl d -> Just d
        _ -> Nothing
    )

rulesFilter :: [Item] -> [Rule]
rulesFilter =
  mapMaybe
    ( \it -> case it of
        Rule r -> Just r
        _ -> Nothing
    )

factsFilter :: [Item] -> [Fact]
factsFilter =
  mapMaybe
    ( \it -> case it of
        Fact f -> Just f
        _ -> Nothing
    )

disjunctionFilter :: [Item] -> [Disjunction]
disjunctionFilter =
  mapMaybe
    ( \it -> case it of
        Disj d -> Just d
        _ -> Nothing
    )

queriesFilter :: [Item] -> [Query]
queriesFilter =
  mapMaybe
    ( \it -> case it of
        Query q -> Just q
        _ -> Nothing
    )
