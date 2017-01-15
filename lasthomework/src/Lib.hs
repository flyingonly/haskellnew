{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Lib where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Map as Map
import Control.Monad.State
import System.Environment
import Data.Text

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
    | FailResult
    | ConsResult MyResult MyResult
    deriving Show

testResult :: MyResult -> Bool
testResult (BoolResult True) = True
testResult _ = False

evalw :: Prog -> Mem -> Mem

evalw (Set p q) mem1 = let f _ = Just (eval mem1 q) in Map.alter f p mem1
evalw Skip mem1 = mem1
evalw (Statmentlist []) mem1 = mem1
evalw (Statmentlist (x:xs)) mem1 = evalw (Statmentlist xs) (evalw x mem1)
evalw (While p q) mem1
    |testResult (eval mem1 p) = evalw (While p q) (evalw q mem1)
    |otherwise = mem1
evalw (If p q s) mem1
    |testResult (eval mem1 p) = (evalw q mem1)
    |otherwise = (evalw s mem1)

statParser :: Parser Statment
statParser = beginParser <|> setParser <|> skipParser <|> ifParser <|> whileParser


beginParser :: Parser Statment
beginParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stats <- many statParser
    lexeme $ char ')'
    return (Statmentlist stats)

variableonlyParser:: Parser Var
variableonlyParser = do
 skipSpace
 a <- let name c = (c >= 'a' && c <= 'z') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in (many (satisfy name))
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
 a <- let name c = (c >= 'a' && c <= 'z') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in (many (satisfy name))
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


eval :: Mem -> Expr -> MyResult
eval _ FalseLit = BoolResult False
eval _ TrueLit = BoolResult True
eval mem1 (VarRef a) = Map.findWithDefault FailResult a mem1
eval mem1 (Not expr) = let BoolResult a = eval mem1 (expr) in BoolResult (not a)
eval mem1 (And expr1 expr2) = let BoolResult a1 = eval mem1 (expr1)
                                  BoolResult a2 = eval mem1 (expr2)
                              in BoolResult (and [a1, a2])
eval mem1 (Or expr1 expr2) = let BoolResult a1 = eval mem1 (expr1)
                                 BoolResult a2 = eval mem1 (expr2)
                             in BoolResult (or [a1, a2])
eval _ (NumLit a) = NumResult a
eval mem1 (Add expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                  NumResult a2 = eval mem1 (expr2)
                              in NumResult (a1 + a2)
eval mem1 (Minus expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                    NumResult a2 = eval mem1 (expr2)
                                in NumResult (a1 - a2)
eval mem1 (Mult expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                   NumResult a2 = eval mem1 (expr2)
                               in NumResult (a1 * a2)
eval mem1 (Div expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                  NumResult a2 = eval mem1 (expr2)
                              in NumResult (a1 / a2)
eval mem1 (Eq expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                 NumResult a2 = eval mem1 (expr2)
                             in BoolResult (a1 == a2)
eval mem1 (Less expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                   NumResult a2 = eval mem1 (expr2)
                               in BoolResult (a1 < a2)
eval mem1 (LessEq expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                     NumResult a2 = eval mem1 (expr2)
                                 in BoolResult (a1 <= a2)
eval mem1 (Greater expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                      NumResult a2 = eval mem1 (expr2)
                                  in BoolResult (a1 > a2)
eval mem1 (GreaterEq expr1 expr2) = let NumResult a1 = eval mem1 (expr1)
                                        NumResult a2 = eval mem1 (expr2)
                                    in BoolResult (a1 >= a2)
eval _ Nil = NilResult
eval _ (CharLit a) = CharResult a
eval mem1 (Cons expr1 expr2) = ConsResult (eval mem1 expr1) (eval mem1 expr2)
eval mem1 (Car expr) = let ConsResult a1 a2 = (eval mem1 expr) in a1
eval mem1 (Cdr expr) = let ConsResult a1 a2 = (eval mem1 expr) in a2

-- designed for parseOnly
--   :: Data.Attoparsec.Text.Parser a
--      -> Data.Text.Internal.Text -> Either String a
evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval Map.empty expr

evalwWithErrorThrowing :: Either String Prog -> String
evalwWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalwWithErrorThrowing (Right prog) = show $ evalw prog Map.empty

data Option = Option {
    inPath :: String,
    outPath :: String
}
    deriving Show

type Parser1 a = StateT [String] IO a

parseFlag :: String -> Parser1 String
parseFlag f = do
    args <- get
    case args of
        [] -> Control.Applicative.empty
        (arg : args')
            | arg == "--" ++ f -> do
                put args'
                return f
            | otherwise -> Control.Applicative.empty

parseField :: String -> Parser1 String
parseField f = do
    parseFlag f
    args <- get
    case args of
        [] -> Control.Applicative.empty
        (arg : args') -> do
            put args'
            return arg

parseInPath :: Parser1 String
parseInPath = parseField "in"

parseOutPath :: Parser1 String
parseOutPath = parseField "out"

parseOption :: Parser1 Option
parseOption = p0 <|> p1 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (Option i o)

    p1 = do
        o <- parseOutPath
        i <- parseInPath
        return (Option i o)

defMain :: IO ()
defMain = do
    args <- getArgs
    (option,_) <- runStateT parseOption args
    inp <- readFile (inPath option)
    instrs <- lines inp
    writeFile (outPath option) inp
    {-writeFile (outPath option) output-}