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
formula = try add <|> try subtract' <|> factor

add :: Parser AST
add = do fop <- factor
         char '+'
         sop <- formula
         return $ Add fop sop

subtract' :: Parser AST
subtract' = do fop <- factor
               char '-'
               sop <- formula
               return $ Sub fop sop

factor :: Parser AST
factor = try multiply <|> try divide <|> pterm

multiply :: Parser AST
multiply = do fop <- term
              char '*'
              sop <- factor
              return $ Mult fop sop

divide :: Parser AST
divide = do fop <- pterm
            char '/'
            sop <- factor
            return $ Div fop sop

pterm :: Parser AST
pterm = try power <|> term

power :: Parser AST
power = do fop <- term
           char '^'
           sop <- pterm
           return $ Pow fop sop

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
