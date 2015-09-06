{-|
Module: AST
Description: Datatypes and functions for modeling and evaluating an AST.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

This module contains datatypes and functions for modeling and evaluating a
algebraic expression's abstract syntax tree.
-}

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

-- | Looks up a variable's value from its name.
varLookup :: String -> Double
varLookup "pi" = pi
varLookup "e" = exp 1
varLookup _ = 0

-- | Looks up a function from its name.
funcLookup :: String -> (Double -> Double)
funcLookup "abs" = abs
funcLookup "signum" = signum
funcLookup "sqrt" = sqrt
funcLookup "log" = log
funcLookup "sin" = sin
funcLookup "cos" = cos
funcLookup "tan" = tan
funcLookup "asin" = asin
funcLookup "acos" = acos
funcLookup "atan" = atan
funcLookup "round" = fromIntegral . round
funcLookup "ceiling" = fromIntegral . ceiling
funcLookup "floor" = fromIntegral . floor
funcLookup _ = const 0

-- | Evaluates an AST given the value for x.
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
