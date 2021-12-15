module Datalog.ProgramTest (test_all) where

import Located
import Test.HUnit
import Test.QuickCheck
import TestCommon
import qualified Datalog.Program as DL
import Parseable (parseFromFile, parser)
import Located (Located (L), locateAt, Location (Location), val)
import Parser (Parser, parse)
import qualified Parser as P
import Pretty (pretty)
import System.Environment (getArgs)
import Datalog.Declaration
import Datalog.Atom
import Datalog.Variable
import Datalog.Constant
import Datalog.Argument
import Datalog.Fact
import Datalog.Literal
import Datalog.Rule
import Datalog.Query
import Common

tProgramParserTest1 :: Test
tProgramParserTest1 =
  "ProgramParserTest1" ~: parse (parser :: Parser DL.Program) "p(X, Y) :- q(X, Y), r(X)."
    ~?= Right
      ( L
          loc1
          ( DL.Program
              []
              [ Datalog.Rule.Rule
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

pathProgram :: DL.Program
pathProgram = DL.Program {DL.declarations=[d1, d2], DL.rules=[rule1, rule2], DL.facts=[fact1, fact2], DL.disjunction=[], DL.queries=[query1]}        
      where
        d1 = Declaration {Datalog.Declaration.predicate="path", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}
        d2 = Declaration {Datalog.Declaration.predicate="edge", Datalog.Declaration.argTypes=[Common.Symbol, Common.Symbol]}        
        var1 = Datalog.Variable.Variable {name="A"}
        var2 = Datalog.Variable.Variable {name="B"}
        var7 = Datalog.Variable.Variable {name="C"}
        a1 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a4 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var7]}
        a5 = Atom {Datalog.Atom.predicate="edge", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a6 = Atom {Datalog.Atom.predicate="path", args=[Datalog.Argument.Variable var2, Datalog.Argument.Variable var7]}
        lit1 = Datalog.Literal.Pos a5
        lit2 = Datalog.Literal.Pos a6
        rule1 = Rule {Datalog.Rule.head=a1, body=[lit1]}
        rule2 = Rule {Datalog.Rule.head=a4, body=[lit2, lit1]}
        const3 = Datalog.Constant.Symbol "Towne"
        const4 = Datalog.Constant.Symbol "Levine"
        const5 = Datalog.Constant.Symbol "Levine"
        const6 = Datalog.Constant.Symbol "Moore"        
        a2 = Atom {Datalog.Atom.predicate="edge", args=[Constant const3, Constant const4]}
        a3 = Atom {Datalog.Atom.predicate="edge", args=[Constant const5, Constant const6]}
        fact1 = Fact {Datalog.Fact.head=a2}
        fact2 = Fact {Datalog.Fact.head=a3}
        a7 = Atom {Datalog.Atom.predicate="path", args=[]}
        query1 = Query {Datalog.Query.query=a7}

loadFile :: FilePath -> IO DL.Program
loadFile fileName = do
  parsed <- parseFromFile fileName
  case parsed of
    Left err -> error $ pretty err
    Right prog -> return prog

pathTest :: IO ()
pathTest = 
  do
    ast <- loadFile "example/path.dl"
    putStrLn "Loaded path program and created its AST. Now comparing it to expected AST."
        -- in putStrLn $ show (DL.facts program1)
    putStrLn $ show (pathProgram == ast)

evenProgram :: DL.Program
evenProgram = DL.Program {DL.declarations=[d1, d2, d3], DL.rules=[rule1, rule2], DL.facts=[fact1, fact2, fact3, fact4, fact5, fact6, fact7, fact8, fact9, fact10, fact11, fact12], DL.disjunction=[], DL.queries=[query1]}        
      where
        d1 = Declaration {Datalog.Declaration.predicate="num", Datalog.Declaration.argTypes=[Common.Integer]}
        d2 = Declaration {Datalog.Declaration.predicate="succ", Datalog.Declaration.argTypes=[Common.Integer, Common.Integer]}
        d3 = Declaration {Datalog.Declaration.predicate="even", Datalog.Declaration.argTypes=[Common.Integer]}
        var1 = Datalog.Variable.Variable {name="E"}
        var2 = Datalog.Variable.Variable {name="0"}
        var7 = Datalog.Variable.Variable {name="E1"}
        a1 = Atom {Datalog.Atom.predicate="even", args=[Datalog.Argument.Variable var2]}
        a4 = Atom {Datalog.Atom.predicate="even", args=[Datalog.Argument.Variable var1]}
        a5 = Atom {Datalog.Atom.predicate="succ", args=[Datalog.Argument.Variable var1, Datalog.Argument.Variable var2]}
        a6 = Atom {Datalog.Atom.predicate="even", args=[Datalog.Argument.Variable var7]}
        a18 = Atom {Datalog.Atom.predicate="succ", args=[Datalog.Argument.Variable var2, Datalog.Argument.Variable var7]}
        lit1 = Datalog.Literal.Pos a1
        lit2 = Datalog.Literal.Pos a4
        lit3 = Datalog.Literal.Pos a5
        lit4 = Datalog.Literal.Pos a6
        lit5 = Datalog.Literal.Pos a18
        rule1 = Rule {Datalog.Rule.head=a1, body=[]}
        rule2 = Rule {Datalog.Rule.head=a4, body=[lit3, lit5, lit4]}
        const3 = Datalog.Constant.Integer 0
        const4 = Datalog.Constant.Integer 1
        const5 = Datalog.Constant.Integer 2
        const6 = Datalog.Constant.Integer 3
        const7 = Datalog.Constant.Integer 4
        const8 = Datalog.Constant.Integer 5
        a2 = Atom {Datalog.Atom.predicate="num", args=[Constant const3]}
        a3 = Atom {Datalog.Atom.predicate="num", args=[Constant const4]}
        a8 = Atom {Datalog.Atom.predicate="num", args=[Constant const5]}
        a9 = Atom {Datalog.Atom.predicate="num", args=[Constant const6]}
        a10 = Atom {Datalog.Atom.predicate="num", args=[Constant const7]}
        a11 = Atom {Datalog.Atom.predicate="num", args=[Constant const8]}
        a12 = Atom {Datalog.Atom.predicate="succ", args=[Constant const3, Constant const4]}
        a13 = Atom {Datalog.Atom.predicate="succ", args=[Constant const4, Constant const5]}
        a14 = Atom {Datalog.Atom.predicate="succ", args=[Constant const5, Constant const6]}
        a15 = Atom {Datalog.Atom.predicate="succ", args=[Constant const6, Constant const7]}
        a16 = Atom {Datalog.Atom.predicate="succ", args=[Constant const7, Constant const8]}
        a17 = Atom {Datalog.Atom.predicate="succ", args=[Constant const8, Constant const3]}        
        fact1 = Fact {Datalog.Fact.head=a2}
        fact2 = Fact {Datalog.Fact.head=a3}
        fact3 = Fact {Datalog.Fact.head=a8}
        fact4 = Fact {Datalog.Fact.head=a9}
        fact5 = Fact {Datalog.Fact.head=a10}
        fact6 = Fact {Datalog.Fact.head=a11}
        fact7 = Fact {Datalog.Fact.head=a12}
        fact8 = Fact {Datalog.Fact.head=a13}
        fact9 = Fact {Datalog.Fact.head=a14}
        fact10 = Fact {Datalog.Fact.head=a15}
        fact11 = Fact {Datalog.Fact.head=a16}
        fact12 = Fact {Datalog.Fact.head=a17}         
        a7 = Atom {Datalog.Atom.predicate="even", args=[]}
        query1 = Query {Datalog.Query.query=a7}    

evenTest :: IO ()
evenTest = 
  do
    ast <- loadFile "example/even.dl"
    putStrLn "Loaded even program and created its AST. Now comparing it to expected AST."
    -- putStrLn $ pretty evenProgram
    putStrLn $ show (DL.facts ast)

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ tProgramParserTests
      ]

-- >>> test_all