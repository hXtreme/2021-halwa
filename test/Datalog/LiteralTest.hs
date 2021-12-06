module Datalog.LiteralTest (test_all) where

import Datalog.Literal
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tLiteralParserTest1 :: Test
tLiteralParserTest1 =
  "tLiteralParserTest1" ~: parse (parser :: Parser Literal) "p(X)"
    ~?= Right (L loc1 (Pos . atomP $ [argX]))

tLiteralParserTests :: Test
tLiteralParserTests =
  "LiteralParserTests" ~: TestList [tLiteralParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tLiteralParserTests]

-- >>> test_all
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
