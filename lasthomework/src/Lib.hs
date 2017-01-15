{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Lib where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Map as Map

type Var = String

data Expr
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr

    | Nil
    | CharLit Char
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

    | VarRef Var
    deriving Show

data Statment
    = Statmentlist [Statment]
    |Set Var Expr
    |Skip
    |If Expr Statment Statment
    |While Expr Statment
    deriving Show

type Prog = Statment
type Mem = Map.Map Var MyResult

data MyResult
    = BoolResult Bool
    | NumResult Double
    | CharResult Char
    | NilResult
    | ConsResult MyResult MyResult
    deriving Show

testResult :: MyResult -> Bool
testResult (BoolResult True) = True
testResult _ = False

evalw :: Prog -> Mem -> Mem

evalw (Set p q) mem1 = let f _ = Just (eval q) in Map.alter f p mem1
evalw Skip mem1 = mem1
evalw (Statmentlist []) mem1 = mem1
evalw (Statmentlist (x:xs)) mem1 = evalw (Statmentlist xs) (evalw x mem1)
evalw (While p q) mem1
    |testResult (eval p) = evalw (While p q) (evalw q mem1)
    |otherwise = mem1
evalw (If p q s) mem1
    |testResult (eval p) = (evalw q mem1)
    |otherwise = (evalw s mem1)

statParser :: Parser Statment
statParser = beginParser <|> setParser <|> skipParser <|> ifParser <|> whileParser


beginParser :: Parser Statment
beginParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stats <- (manyTill statParser (char ')'))
    return (Statmentlist stats)

variableonlyParser:: Parser Var
variableonlyParser = do
 skipSpace
 a <- (manyTill anyChar (char ' '))
 return a

setParser :: Parser Statment
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    v <- variableonlyParser
    skipSpace
    expr1 <- exprParser
    lexeme $ char ')'
    return (Set v expr1)

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

{-makevectorParser :: Parser Statment
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
    return (Return expr)-}


exprParser :: Parser Expr
exprParser = nilParser <|> falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser
            <|> numParser <|> addParser <|> minusParser <|> multParser <|> divParser
            <|> eqParser <|> lessParser <|> lesseqParser <|> greaterParser <|> greatereqParser 
            <|> variableParser

variableParser:: Parser Expr
variableParser = do
 skipSpace
 a <- (manyTill anyChar (char ' '))
 return (VarRef a)

nilParser :: Parser Expr
nilParser = lexeme $ string "nil" $> Nil

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

charParser :: Parser Expr
charParser = do
 lexeme $ char '\''
 a <- anyChar
 char '\''
 return (CharLit a)

trans :: [Char] -> Expr
trans [] = Nil
trans (x:xs) = (Cons (CharLit x) (trans xs))

stringParser:: Parser Expr
stringParser = do
 lexeme $ char '\"'
 a <- (manyTill anyChar (char '\"'))
 return (trans a)

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


eval :: Expr -> MyResult
eval FalseLit = BoolResult False
eval TrueLit = BoolResult True
eval (Not expr) = let BoolResult a = eval (expr) in BoolResult (not a)
eval (And expr1 expr2) = let BoolResult a1 = eval (expr1)
                             BoolResult a2 = eval (expr2)
                         in BoolResult (and [a1, a2])
eval (Or expr1 expr2) = let BoolResult a1 = eval (expr1)
                            BoolResult a2 = eval (expr2)
                        in BoolResult (or [a1, a2])
eval (NumLit a) = NumResult a
eval (Add expr1 expr2) = let NumResult a1 = eval (expr1)
                             NumResult a2 = eval (expr2)
                         in NumResult (a1 + a2)
eval (Minus expr1 expr2) = let NumResult a1 = eval (expr1)
                               NumResult a2 = eval (expr2)
                           in NumResult (a1 - a2)
eval (Mult expr1 expr2) = let NumResult a1 = eval (expr1)
                              NumResult a2 = eval (expr2)
                          in NumResult (a1 * a2)
eval (Div expr1 expr2) = let NumResult a1 = eval (expr1)
                             NumResult a2 = eval (expr2)
                         in NumResult (a1 / a2)
eval (Eq expr1 expr2) = let NumResult a1 = eval (expr1)
                            NumResult a2 = eval (expr2)
                        in BoolResult (a1 == a2)
eval (Less expr1 expr2) = let NumResult a1 = eval (expr1)
                              NumResult a2 = eval (expr2)
                          in BoolResult (a1 < a2)
eval (LessEq expr1 expr2) = let NumResult a1 = eval (expr1)
                                NumResult a2 = eval (expr2)
                            in BoolResult (a1 <= a2)
eval (Greater expr1 expr2) = let NumResult a1 = eval (expr1)
                                 NumResult a2 = eval (expr2)
                             in BoolResult (a1 > a2)
eval (GreaterEq expr1 expr2) = let NumResult a1 = eval (expr1)
                                   NumResult a2 = eval (expr2)
                               in BoolResult (a1 >= a2)
eval Nil = NilResult
eval (CharLit a) = CharResult a
eval (Cons expr1 expr2) = ConsResult (eval expr1) (eval expr2)
eval (Car expr) = let ConsResult a1 a2 = eval expr in a1
eval (Cdr expr) = let ConsResult a1 a2 = eval expr in a2

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
