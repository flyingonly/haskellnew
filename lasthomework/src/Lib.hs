{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Lib where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Expr
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr

    | Nil
    | Char_literal Char
    | String_literal [Char]
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr

    | NumLit Double
    | Add Expr Expr
    | Minus Expr Expr
    | Mult Expr Expr
    | Div Expr Expr

    | Eq Expr Expr
    | Less Expr Expr
    | LessEq Expr Expr
    | Greater Expr Expr
    | GreaterEq Expr Expr

    | Variable
    | Vectorref Expr Expr
    | Functionname [Expr]
    deriving Show

data Statment
    = Begin [Statment]
    |Set Expr Expr
    |Skip
    |If Expr Statment Statment
    |While Expr Statment
    |Makevector Expr Expr
    |Vectorset Expr Expr Expr
    |Return Expr

data Function = Define Expr Statment

data Result1
    = BoolResult Bool
    | NumResult Double
    | NilResult
    | ConsResult Result1 Result1

statParser :: Parser Statment
statParser = beginParser <|> setParser <|> skipParser <|> ifParser <|> whileParser
            <|> makevectorParser <|> vectorsetParser <|> returnParser


beginParser :: Parser Statment
beginParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stats <- many statParser
    lexeme $ char ')'
    return (Begin stats)

setParser :: Parser Statment
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Set expr expr1)

skipParser :: Parser Statment
skipParser = lexeme $ string "skip" $> Skip

ifParser :: Parser Statment
ifParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    skipSpace
    stat <- statParser
    skipSpace
    stat1 <- statParser
    lexeme $ char ')'
    return (If expr stat stat1)

whileParser :: Parser Statment
whileParser = do
    lexeme $ char '('
    lexeme $ string "while"
    expr <- exprParser
    skipSpace
    stat <- statParser
    lexeme $ char ')'
    return (While expr stat)

makevectorParser :: Parser Statment
makevectorParser = do
    lexeme $ char '('
    lexeme $ string "make-vector"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Makevector expr expr1)

vectorsetParser :: Parser Statment
vectorsetParser = do
    lexeme $ char '('
    lexeme $ string "vector-set!"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    skipSpace
    expr2 <- exprParser
    lexeme $ char ')'
    return (Vectorset expr expr1 expr2)

returnParser :: Parser Statment
returnParser = do
    lexeme $ char '('
    lexeme $ string "return"
    expr <- exprParser
    lexeme $ char ')'
    return (Return expr)


exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser
            <|> numParser <|> addParser <|> minusParser <|> multParser <|> divParser
            <|> eqParser <|> lessParser <|> lesseqParser <|> greaterParser <|> greatereqParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

charParser :: Parser Expr
charParser = do
 lexeme $ char '\''
 a <- anyChar
 char '\''
 return (Char_literal a)

stringParser:: Parser Expr
stringParser = do
 lexeme $ char '\"'
 a <- many anyChar
 char '\"'
 return (String_literal a)

consParser:: Parser Expr
consParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Cons expr expr1)

carParser:: Parser Expr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ char ')'
    return (Car expr)

cdrParser:: Parser Expr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (And expr expr1)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Or expr expr1)

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

numParser :: Parser Expr
numParser = do
    skipSpace
    num <- double
    return (NumLit num)

addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Add expr expr1)

minusParser :: Parser Expr
minusParser = do
    lexeme $ char '('
    lexeme $ char '-'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Minus expr expr1)

multParser :: Parser Expr
multParser = do
    lexeme $ char '('
    lexeme $ char '*'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Mult expr expr1)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Div expr expr1)

eqParser :: Parser Expr
eqParser = do
    lexeme $ char '('
    lexeme $ char '='
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Eq expr expr1)

lessParser :: Parser Expr
lessParser = do
    lexeme $ char '('
    lexeme $ char '<'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Less expr expr1)

lesseqParser :: Parser Expr
lesseqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (LessEq expr expr1)

greaterParser :: Parser Expr
greaterParser = do
    lexeme $ char '('
    lexeme $ char '>'
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Greater expr expr1)

greatereqParser :: Parser Expr
greatereqParser = do
    lexeme $ char '('
    lexeme $ string ">="
    skipSpace
    expr <- exprParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (GreaterEq expr expr1)


eval :: Expr -> Bool
eval FalseLit = False
eval TrueLit = True
eval (Not p) = not $ eval p
eval (And p q) = and [(eval p),(eval q)]
eval (Or p q) = or [(eval p),(eval q)]

-- designed for parseOnly
--   :: Data.Attoparsec.Text.Parser a
--      -> Data.Text.Internal.Text -> Either String a
evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr

defMain :: IO ()
defMain = do
    putStrLn $ show $ parseOnly notParser "(not True)"
    putStrLn $ show $ parse notParser "(not True)"
    putStrLn "-------"
    putStrLn $ show $ parseOnly notParser "(nXXX True)"
    putStrLn $ show $ parse notParser "(nXXX True)"
    putStrLn "-------"
    putStrLn $ show $ parseOnly notParser "(not Tr"
    putStrLn $ show $ parse notParser "(not Tr"
    putStrLn "-------"
    putStrLn $ show $ parseOnly notParser "(not True)   MORE"
    putStrLn $ show $ parse notParser "(not True)   MORE"
    putStrLn "--------------"
    putStrLn $ show $ parseOnly exprParser "(not True)"
    putStrLn $ show $ parse exprParser "(not True)"
    putStrLn "-------"
    putStrLn $ show $ parseOnly exprParser "(nXXX True)"
    putStrLn $ show $ parse exprParser "(nXXX True)"
    putStrLn "-------"
    putStrLn $ show $ parseOnly exprParser "(not Tr"
    putStrLn $ show $ parse exprParser "(not Tr"
    putStrLn "-------"
    putStrLn $ show $ parseOnly exprParser "(not True)   MORE"
    putStrLn $ show $ parse exprParser "(not True)   MORE"
    putStrLn "--------------"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not True)"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not Tr"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(nXXX True)"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not True)   MORE"
