module Datalog.RuleTest (test_all) where

import Datalog.Atom
import Datalog.Rule
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tRuleParserTest1 :: Test
tRuleParserTest1 =
  "tRuleParserTest1" ~: parse (parser :: Parser Rule) "p(X,Y) :- q(X,Y)"
    ~?= Right
      ( L
          loc1
          (Rule (atomP argsXY) [lit . atomQ $ argsXY])
      )

tRuleParserTest2 :: Test
tRuleParserTest2 =
  "tRuleParserTest2" ~: parse (parser :: Parser Rule) "p(X,Y) :- q(X,Y), r(X,Y)"
    ~?= Right
      ( L
          loc1
          (Rule (atomP argsXY) [lit . atomQ $ argsXY, lit . atomR $ argsXY])
      )

tRuleParserTests :: Test
tRuleParserTests =
  "RuleParserTests"
    ~: TestList
      [ tRuleParserTest1,
        tRuleParserTest2
      ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tRuleParserTests]

-- >>> test_all
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
