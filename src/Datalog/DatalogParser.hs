module Datalog.DatalogParser where

import qualified Common as C
import Control.Applicative
import Data.Functor
import qualified Datalog.Atom as DL.Atom
import qualified Datalog.Argument as DL.Argument
import qualified Datalog.Common as DL.Common
import qualified Datalog.Declaration as DL.Declaration
import qualified Datalog.Program as DL.Program
import Parser (Parser, parse)
import qualified Parser as P
import Parseable (parser)

data Item
  = Empty
  | Comment String
  | Decl DL.Program.Declaration
  | Fact DL.Program.Fact
  | Rule DL.Program.Rule
  | Query DL.Program.Query
  deriving (Eq, Show)

-- >>> parse (many (constP "foo" "bar")) "foo  boo foo"
-- Right (L (Location 1 1) ["bar"])

keyWordDecl :: Parser ()
keyWordDecl = P.stringP "decl"

keyWordQuery :: Parser ()
keyWordQuery = P.stringP "query"

keyWordTrue :: Parser Bool
keyWordTrue = P.constP "true" True

keyWordFalse :: Parser Bool
keyWordFalse = P.constP "false" False

-- >>> parse (lowerCaseName) "foo(bar)"
-- Right (L (Location 1 1) "foo")

-- >>> parse (lowerCaseName) "foo  (bar)"
-- Right (L (Location 1 1) "foo")

-- | Parse a lower-case name. regex: [a-z][a-zA-Z0-9_]*
lowerCaseName :: Parser String
lowerCaseName = P.filter DL.Common.isNotKeyWord . P.wsP $ P.snakeCaseWord

-- >>> parse (initialUpperCaseName) "foo(bar)"
-- Left "No parses"

-- >>> parse (initialUpperCaseName) "Foo(bar)"
-- Right (L (Location 1 1) "Foo")

-- | Parse a name that starts with an uppercase letter. regex: [a-z][a-zA-Z0-9_]*
initialUpperCaseName :: Parser String
initialUpperCaseName = P.filter DL.Common.isNotKeyWord $ P.wsP P.titleCaseWord

atomP :: Parser DL.Program.Atom
atomP = DL.Atom.Atom <$> lowerCaseName <*> argsP
  where
    name = lowerCaseName
    argsP = P.parensP $ P.sepBy argumentP P.commaP

commentP :: Parser Item
commentP = Comment <$> (P.stringP "//" *> P.wsP P.line)

-- >>> parse declarationP "decl foo"
-- Right (L (Location 1 1) (Declaration {predicate = "foo", argTypes = []}))

-- >>> parse declarationP "decl foo()"
-- Right (L (Location 1 1) (Declaration {predicate = "foo", argTypes = []}))

-- >>> parse declarationP "decl foo(Symbol)"
-- Right (L (Location 1 1) (Declaration {predicate = "foo", argTypes = [Symbol]}))

-- >>> parse declarationP "decl foo (Symbol, Int, Int)"
-- Right (L (Location 1 1) (Declaration {predicate = "foo", argTypes = [Symbol,Integer,Integer]}))

-- | Parse a declaration statement.
declarationP :: Parser DL.Program.Declaration
declarationP = DL.Declaration.Declaration <$> (keyWordDecl *> name) <*> argTypes
  where
    name = lowerCaseName
    argTypes = parens (P.sepBy typeP commaP) <|> pure []
