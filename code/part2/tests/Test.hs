-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "Print test" $
    execute[ SExp (Call "print" [Const (IntVal 42), Const (StringVal "foo"),
                                        Const (ListVal [TrueVal, ListVal []]), Const (IntVal (-1))])]
      @?= (["42 foo [True, []] -1"], Nothing),
   testCase "execute test.ast" $
     do pgm <- read <$> readFile "test/test.ast"
        out <- readFile "test/test.out"
        execute pgm @?= (lines out, Nothing),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)]