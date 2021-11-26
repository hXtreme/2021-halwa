module Datalog.DatalogParser where

import qualified Common as C
import Control.Applicative
import Data.Functor
import qualified Datalog.Atom as DL.Atom
import qualified Datalog.Declaration as DL.Declaration
import qualified Datalog.Program as DL.Program
import Parser (Parser, parse)
import qualified Parser as P

data Item
  = Empty
  | Comment String
  | Decl DL.Program.Declaration
  | Fact DL.Program.Fact
  | Rule DL.Program.Rule
  | Query DL.Program.Query
  deriving (Eq, Show)

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

constP :: String -> a -> Parser a
constP s x = (wsP . P.string) s $> x

-- >>> parse (many (constP "foo" "bar")) "foo  boo foo"
-- Right (L (Location 1 1) ["bar"])

stringP :: String -> Parser ()
stringP s = constP s ()

commaP :: Parser ()
commaP = stringP ","

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

reserved :: [String]
reserved =
  [ "decl",
    "query",
    "Symbol",
    "Int",
    "Bool",
    "String",
    "true",
    "false"
  ]

isReserved :: String -> Bool
isReserved = (`elem` reserved)

keyWordDecl :: Parser ()
keyWordDecl = stringP "decl"

keyWordQuery :: Parser ()
keyWordQuery = stringP "query"

keyWordTrue :: Parser Bool
keyWordTrue = constP "true" True

keyWordFalse :: Parser Bool
keyWordFalse = constP "false" False

typeP :: Parser C.Type
typeP =
  constP "Symbol" C.Symbol
    <|> constP "Int" C.Integer
    <|> constP "Bool" C.Boolean
    <|> constP "String" C.String

-- >>> parse (lowerCaseName) "foo(bar)"
-- Right (L (Location 1 1) "foo")

-- >>> parse (lowerCaseName) "foo  (bar)"
-- Right (L (Location 1 1) "foo")

-- | Parse a lower-case name. regex: [a-z][a-zA-Z0-9_]*
lowerCaseName :: Parser String
lowerCaseName = P.filter notReserved $ wsP $ (:) <$> P.lower <*> many (P.lower <|> P.digit <|> P.char '_')
  where
    notReserved = not . isReserved

-- >>> parse (initialUpperCaseName) "foo(bar)"
-- Left "No parses"

-- >>> parse (initialUpperCaseName) "Foo(bar)"
-- Right (L (Location 1 1) "Foo")

-- | Parse a name that starts with an uppercase letter. regex: [a-z][a-zA-Z0-9_]*
initialUpperCaseName :: Parser String
initialUpperCaseName = P.filter notReserved $ wsP $ (:) <$> P.upper <*> many (P.alpha <|> P.digit <|> P.char '_')
  where
    notReserved = not . isReserved

commentP :: Parser Item
commentP = Comment <$> (stringP "//" *> wsP P.line)

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
