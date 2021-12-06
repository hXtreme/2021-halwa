module Datalog.ProgramTest (test_all) where

import Datalog.Program
import Located
import Parseable (Parseable (parser))
import Parser (Parser, parse)
import Test.HUnit
import Test.QuickCheck
import TestCommon
import qualified Datalog.Rule as DL.Rule

tProgramParserTest1 :: Test
tProgramParserTest1 =
  "ProgramParserTest1" ~: parse (parser :: Parser Program) "p(X, Y) :- q(X, Y), r(X)."
    ~?= Right
      ( L
          loc1
          ( Program
              []
              [ DL.Rule.Rule
                  (atomP argsXY)
                  [lit (atomQ argsXY), lit (atomR [argX])]
              ]
              []
              []
              []
          )
      )

tProgramParserTests :: Test
tProgramParserTests =
  "tProgramParserTests"
    ~: TestList [tProgramParserTest1]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ tProgramParserTests
      ]

-- >>> test_all