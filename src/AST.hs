
module AST where

import qualified Data.Map as M

-- | The AST for a math expression.
data AST = Const Double
         | Var String
         | Func String AST
         | Add AST AST
         | Sub AST AST
         | Mult AST AST
         | Div AST AST
         | Pow AST AST
         | Neg AST
         deriving (Eq, Show)

varLookup :: String -> Double
varLookup "pi" = pi
varLookup "e" = exp 1
varLookup _ = 0

funcLookup :: String -> (Double -> Double)
funcLookup "sqrt" = sqrt
funcLookup "log" = log
funcLookup "sin" = sin
funcLookup "cos" = cos
funcLookup "tan" = tan
funcLookup "asin" = asin
funcLookup "acos" = acos
funcLookup "atan" = atan
funcLookup _ = const 0

evalAST :: AST -> Double -> Double
evalAST (Const c) _ = c
evalAST (Var "x") x = x
evalAST (Var v) _ = varLookup v
evalAST (Func f a) x = (funcLookup f) (evalAST a x)
evalAST (Add a b) x = (evalAST a x) + (evalAST b x)
evalAST (Sub a b) x = (evalAST a x) - (evalAST b x)
evalAST (Mult a b) x = (evalAST a x) * (evalAST b x)
evalAST (Div a b) x = (evalAST a x) / (evalAST b x)
evalAST (Pow a b) x = (evalAST a x) ** (evalAST b x)
evalAST (Neg a) x = negate $ evalAST a x
