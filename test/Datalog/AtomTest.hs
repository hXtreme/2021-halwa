module Datalog.AtomTest (test_all) where

import qualified Datalog.Argument as Arg
import Datalog.Atom
import qualified Datalog.Variable as Var
import Located
import Parseable (parser)
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tAtomParserTest1 :: Test
tAtomParserTest1 =
  "AtomParserTest1" ~: parse (parser :: Parser Atom) "p(X,Y)"
    ~?= Right (L loc1 (atomP argsXY))

tAtomParserTests :: Test
tAtomParserTests =
  "AtomParserTests" ~: TestList [tAtomParserTest1]

-- >>> P.parse (parser :: P.Parser Atom) "p(X,Y)"
-- Right (L (Location 1 1) (Atom {predicate = "p", args = []}))

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tAtomParserTests]

-- >>> test_all
