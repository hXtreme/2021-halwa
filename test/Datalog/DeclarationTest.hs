module Datalog.DeclarationTest (test_all) where

import Datalog.Declaration
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon

tDeclarationParserTest1 :: Test
tDeclarationParserTest1 =
  "DeclarationParserTest1" ~: parse (parser :: Parser Declaration) "decl r(Bool, Bool)"
    ~?= Right
      ( L
          loc1
          (Declaration "r" argTypesBoolBool)
      )

tDeclarationParserTests :: Test
tDeclarationParserTests =
  "DeclarationParserTests" ~: TestList [tDeclarationParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList [tDeclarationParserTests]

--  >>> test_all
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
