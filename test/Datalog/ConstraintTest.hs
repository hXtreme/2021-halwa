module Datalog.ConstraintTest (test_all) where

import qualified Common as C -- (UnOP (..), BinOP (..))
import Datalog.Constraint
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tUnaryConstraintParserTest1 :: Test
tUnaryConstraintParserTest1 =
  "UnaryConstraintParserTest1" ~: parse (parser :: Parser Constraint) "!X"
    ~?= Right
      (L loc1 (UnaryConstraint C.Not argX))

tUnaryConstraintParserTests :: Test
tUnaryConstraintParserTests =
  "UnaryConstraintParserTests" ~: TestList [tUnaryConstraintParserTest1]


tBinaryConstraintParserTest1 :: Test
tBinaryConstraintParserTest1 =
    "BinaryConstraintParserTest1" ~: parse (parser :: Parser Constraint) "X < Y"
        ~?= Right
        (L loc1 (BinaryConstraint argX C.Lt argY))

tBinaryConstraintParserTests :: Test
tBinaryConstraintParserTests =
  "BinaryConstraintParserTests" ~: TestList [tBinaryConstraintParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tUnaryConstraintParserTests, tBinaryConstraintParserTests]

-- >>> test_all
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
