module Common
  ( Type (..),
    BinOP (..),
    UnOP (..),
  )
where

import Control.Applicative
import Parseable (Parseable, parser)
import qualified Parser as P
import Pretty (Pretty (pretty))

data Type
  = Symbol
  | Integer
  | Boolean
  | String
  deriving (Eq, Show)

instance Parseable Type where
  parser =
    P.constP "Symbol" Symbol
      <|> P.constP "Int" Integer
      <|> P.constP "Bool" Boolean
      <|> P.constP "String" String

instance Pretty Type where
  pretty = show

data BinOP
  = Eq
  | Ne
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Add
  | Sub
  | Mult
  | Div
  deriving (Eq, Show)

instance Parseable BinOP where
  parser =
    P.constP "=" Eq
      <|> P.constP "!=" Ne
      <|> P.constP "<=" Leq
      <|> P.constP "<" Lt
      <|> P.constP ">=" Geq
      <|> P.constP ">" Gt
      <|> P.constP "&&" And
      <|> P.constP "||" Or
      <|> P.constP "+" Add
      <|> P.constP "-" Sub
      <|> P.constP "*" Mult
      <|> P.constP "/" Div

instance Pretty BinOP where
  pretty op = case op of
    Eq -> "="
    Ne -> "!="
    Leq -> "<="
    Lt -> "<"
    Geq -> ">="
    Gt -> ">"
    And -> "&&"
    Or -> "||"
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Div -> "/"

data UnOP
  = Not
  | Pos
  | Neg
  deriving (Eq, Show)

instance Parseable UnOP where
  parser =
    P.constP "!" Not
      <|> P.constP "+" Pos
      <|> P.constP "-" Neg

instance Pretty UnOP where
  pretty op = case op of
    Not -> "!"
    Pos -> "+"
    Neg -> "-"
