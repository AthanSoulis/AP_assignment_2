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
truthy NoneVal        = False
truthy FalseVal       = False
truthy (IntVal 0)     = False
truthy (StringVal "") = False
truthy (ListVal [])   = False
truthy _              = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus    (IntVal v1) (IntVal v2)       = Right (IntVal (v1 + v2))
operate Minus   (IntVal v1) (IntVal v2)       = Right (IntVal (v1 - v2))
operate Times   (IntVal v1) (IntVal v2)       = Right (IntVal (v1 * v2))
operate Div     (IntVal v1) (IntVal v2)       = if v2 /= 0 then Right (IntVal (v1 `div` v2)) 
  else Left $ "Attempted division by zero in " ++ show v1 ++ " `div` " ++ show v2
operate Mod     (IntVal v1) (IntVal v2)       = if v2 /= 0 then Right (IntVal (v1 `mod` v2))
  else Left $ "Attempted division by zero in " ++ show v1 ++ " `mod` " ++ show v2
operate Eq      (IntVal v1) (IntVal v2)       = Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq      (StringVal v1) (StringVal v2) = Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq      (ListVal v1) (ListVal v2)     = Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq       v1 v2                        = Right (if v1 == v2 then TrueVal else FalseVal)
operate Less    (IntVal v1) (IntVal v2)       = Right (if v1 < v2 then TrueVal else FalseVal)
operate Greater (IntVal v1) (IntVal v2)       = Right (if v1 > v2 then TrueVal else FalseVal)
operate In       _ (ListVal [])               = Right FalseVal
operate In       v1 (ListVal (v:vs))          = case operate Eq v1 v of 
  (Left _) -> Right FalseVal
  (Right x) -> if x == TrueVal then Right TrueVal else operate In v1 (ListVal vs)
operate o v1 v2 = Left $ "Operator " ++ show o ++ " is applied to inappropriate arguments " ++ show v1 ++ ", " ++ show v2

--NOTE: COULD BE WAY CLEANER
apply :: FName -> [Value] -> Comp Value
apply "range" [n2] = apply "range" [IntVal 0, n2, IntVal 1]
apply "range" [n1, n2] = apply "range" [n1, n2, IntVal 1]
apply "range" [IntVal n1, IntVal n2, IntVal n3]
  | n3 == 0 = Comp (\_ -> (Left (EBadArg $ show n3), mempty))
  | n3 > 0  = if n1 >= n2 then Comp (\_ -> (Right $ ListVal [], mempty)) else 
    Comp (\_ -> (Right $ ListVal $ map IntVal (takeWhile (< n2) (iterate (+ n3) n1)), mempty))
  | n3 < 0  = if n1 <= n2 then Comp (\_ -> (Right $ ListVal [], mempty)) else 
    Comp (\_ -> (Right $ ListVal $ map IntVal (takeWhile (> n2) (iterate (+ n3) n1)), mempty))
apply "range" x = Comp (\_ -> (Left $ EBadArg (show x), mempty))

apply "print" x = Comp (\_ -> (Right NoneVal, out)) where
  out = if x /= [] then [tail $ foldl (++) "" (map toString x)] else [""]

apply x _ = Comp (\_ -> (Left (EBadFun x), mempty))

--NOTE: LISTVAL NOT WORKING
toString :: Value -> String
toString NoneVal = " None"
toString TrueVal = " True"
toString FalseVal = " False"
toString (IntVal n) = " " ++ show n
toString (StringVal s) = " " ++ s
toString (ListVal []) = " []"
toString (ListVal (x:xs)) = undefined

  --if x /= [] then foldl (++) "" (map toString x) else "[]"
--  if x /= [] then " [" ++ foldl (++) "" (map toString x) ++ "]" else "[]"

-- output' :: String -> Comp Value
-- output' s = Comp (\_ -> (Right NoneVal, [s]))

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
