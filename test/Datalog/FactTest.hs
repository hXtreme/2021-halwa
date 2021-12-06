module Datalog.FactTest (test_all) where

import Datalog.Fact
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tFactParserTest1 :: Test
tFactParserTest1 =
  "FactParserTest1" ~: parse (parser :: Parser Fact) "q(1,2)"
    ~?= Right
      ( L
          loc1
          (Fact (atomQ args12))
      )

tFactParserTests :: Test
tFactParserTests =
  "FactParserTests" ~: TestList [tFactParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tFactParserTests]

-- >>> test_all
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
