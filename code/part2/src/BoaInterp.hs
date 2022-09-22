-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f  = Comp (\env -> case runComp m env of 
              (Left e, out1)  -> (Left e, out1)
              (Right a, out1) -> case runComp (f a) env of
                (Left e2, out2)  -> (Left e2, out1 ++ out2)
                (Right x, out2) -> (Right x, out1 ++ out2))
                

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap




-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\_ -> (Left re, mempty))

look :: VName -> Comp Value
look x = Comp (\env -> case lookup x env of
          Nothing -> (Left (EBadVar x), mempty)
          Just y  -> (Right y, mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp (\env -> case runComp m ((x, v):env) of
                      (Left e, out)  -> (Left e, out)
                      (Right a, out) -> (Right a, out))

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy = undefined

operate :: Op -> Value -> Value -> Either String Value
operate = undefined

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
