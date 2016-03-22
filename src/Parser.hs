{-|
Module: Parser
Description: Parsers for algebraic expressions.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

This module contains functions parsing algebraic expressions.
-}

module Parser where

import Text.ParserCombinators.Parsec

import AST

-- | Parses a string into an AST for an algebraic expression.
parseAST :: String -> Either ParseError AST
parseAST str = parse formula "Formula" (stripWhitespace str)

-- | Strips *all* whitespace from a string.
stripWhitespace :: String -> String
stripWhitespace [] = []
stripWhitespace (' ':str) = stripWhitespace str
stripWhitespace ('\t':str) = stripWhitespace str
stripWhitespace ('\n':str) = stripWhitespace str
stripWhitespace ('\r':str) = stripWhitespace str
stripWhitespace (c:str) = c : stripWhitespace str

formula :: Parser AST
formula = do x <- factor
             op <- optionMaybe (add <|> subtract')
             case op of
              Just (Left y) -> return $ Add x y
              Just (Right y) -> return $ Sub x y
              Nothing -> return $ x

add :: Parser (Either AST AST)
add = do char '+'
         x <- formula
         return $ Left x

subtract' :: Parser (Either AST AST)
subtract' = do char '-'
               x <- formula
               return $ Right x

factor :: Parser AST
factor = do x <- pterm
            op <- optionMaybe (multiply <|> divide)
            case op of
             Just (Left y) -> return $ Mult x y
             Just (Right y) -> return $ Div x y
             Nothing -> return $ x

multiply :: Parser (Either AST AST)
multiply = do char '*'
              x <- factor
              return $ Left x

divide :: Parser (Either AST AST)
divide = do char '/'
            x <- factor
            return $ Right x

pterm :: Parser AST
pterm = do x <- term
           op <- optionMaybe (char '^' >> pterm)
           case op of
            Just y -> return $ Pow x y
            Nothing -> return $ x

term :: Parser AST
term = try function <|> variable <|> constant <|> group

group :: Parser AST
group = do char '('
           ast <- formula
           char ')'
           return ast

function :: Parser AST
function = do name <- many1 letter
              char '('
              a <- formula
              char ')'
              return $ Func name a

variable :: Parser AST
variable = do name <- many1 letter
              return $ Var name

constant :: Parser AST
constant = do neg <- option "" (string "-")
              fdigs <- many1 digit
              rdigs <- option "" (do char '.'
                                     digs <- many digit
                                     return $ "." ++ digs)
              return $ Const $ read (neg ++ fdigs ++ rdigs)
