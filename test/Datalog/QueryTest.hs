module Datalog.QueryTest (test_all) where

import Datalog.Query
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tQueryParserTest1 :: Test
tQueryParserTest1 =
  "QueryParserTest1" ~: parse (parser :: Parser Query) "query p"
    ~?= Right
      (L loc1 (Query (atomP [])))

tQueryParserTests :: Test
tQueryParserTests =
  "QueryParserTests" ~: TestList [tQueryParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
  TestList [tQueryParserTests]

-- >>> test_all
-- Counts {cases = 1, tried = 1, errors = 0, failures = 1}
