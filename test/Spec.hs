import Control.Monad (when)
import qualified Datalog.AtomTest as AtomTest
import qualified Datalog.ConstraintTest as ConstraintTest
import qualified Datalog.DeclarationTest as DeclarationTest
import qualified Datalog.FactTest as FactTest
import qualified Datalog.LiteralTest as LiteralTest
import qualified Datalog.ProgramTest as ProgramTest
import qualified Datalog.QueryTest as QueryTest
import qualified Datalog.RuleTest as RuleTest
import System.Exit
import Test.HUnit
import Test.HUnit.Base (Counts)
import Test.QuickCheck

exitOnErrorOrFailure :: IO Counts -> IO ()
exitOnErrorOrFailure counts = do
  c <- counts
  when (errors c > 0 || failures c > 0) $ exitWith (ExitFailure 1)

main :: IO ()
main =
  do
    putStrLn "Running Atom tests..."
    exitOnErrorOrFailure AtomTest.test_all
    putStrLn "Running Fact tests..."
    exitOnErrorOrFailure FactTest.test_all
    putStrLn "Running Literal tests..."
    exitOnErrorOrFailure LiteralTest.test_all
    putStrLn "Running Constraint tests..."
    exitOnErrorOrFailure ConstraintTest.test_all
    putStrLn "Running Declaration tests..."
    exitOnErrorOrFailure DeclarationTest.test_all
    putStrLn "Running Rule tests..."
    exitOnErrorOrFailure RuleTest.test_all
    putStrLn "Running Query tests..."
    exitOnErrorOrFailure QueryTest.test_all
    putStrLn "Running Program tests..."
    exitOnErrorOrFailure ProgramTest.test_all
    exitSuccess
