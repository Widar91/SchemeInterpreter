module Parser where

import Grammar

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array


readOrError :: Parser a -> String -> ErrorM a
readOrError parser input = case parse parser "lisp" input of
    Left  e -> throwError $ Parser e
    Right v -> return v 

readExpr     = readOrError parseExpr
readExprList = readOrError (sepBy parseExpr (spaces1 <|> eof))


-------------
-- Parsers
-------------

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parsePrefix
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> parseDecimal
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuoted
         <|> parseList

parseList :: Parser LispVal
parseList = do 
    char '('
    x <- parseList'
    char ')'
    return x

parseList' :: Parser LispVal
parseList' = do 
    h <- spaces >> sepEndBy parseExpr spaces1
    (char '.' >> spaces1 >> parseExpr >>= \t -> return (DottedList h t)) <|> return (List h)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseAtom :: Parser LispVal
parseAtom = do
    h <- letter <|> symbol
    t <- many (letter <|> symbol <|> digit)
    let atom = h:t
    return $ Atom atom

parsePrefix :: Parser LispVal
parsePrefix = do
    char '#'
    parseBool <|> parseNumberBase <|> parseChar <|> parseVector

parseBool :: Parser LispVal
parseBool = do
    l <- oneOf "tf"
    notFollowedBy (letter <|> symbol <|> digit)
    return $ case l of 
        't' -> Bool True
        'f' -> Bool False

parseNumberBase :: Parser LispVal
parseNumberBase = do
    l <- oneOf "boxd"
    case l of 
        'x' -> liftM (Number . fst . head . readHex) $ many1 (digit <|> oneOf "aAbBcCdDeEfF")
        'o' -> liftM (Number . fst . head . readOct) $ many1 (oneOf "01234567")
        'b' -> liftM (Number . bin2dec) $ many1 (oneOf "01")
        'd' -> parseDecimal

parseChar :: Parser LispVal
parseChar = do 
    char '\\'
    c <- many1 letter
    return $ case c of 
        "newline" -> Char '\n'
        "tab"     -> Char '\t'
        "space"   -> Char ' '
        [c']      -> Char c' 

parseVector :: Parser LispVal
parseVector = do
    char '('
    xs <- sepBy parseExpr spaces1
    char ')'
    return $ Vector (listArray (0, length xs - 1) xs)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf ['"','\\'] <|> parseEscaped)
    char '"'
    return $ String x

parseEscaped :: Parser Char
parseEscaped = do
    char '\\'
    c <- oneOf ['\\', '"', 't', 'n', 'r']
    return $ case c of
        'n'       -> '\n'
        't'       -> '\t'
        'r'       -> '\r'
        otherwise -> c

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do 
    n <- many1 digit
    char '.'
    d <- many1 digit
    return (Float . fst . head . readFloat $ n ++ "." ++ d)

parseRatio :: Parser LispVal
parseRatio = do
    n <- many1 digit
    char '/'
    d <- many1 digit
    return $ Ratio (read n % read d)

parseComplex :: Parser LispVal
parseComplex = do 
    n <- try parseFloat <|> parseDecimal
    char '+'
    i <- try parseFloat <|> parseDecimal
    char 'i' 
    return $ Complex (toDouble n :+ toDouble i)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space


-- Helper Functions
bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

toDouble :: LispVal -> Double
toDouble (Float f)  = f
toDouble (Number n) = fromIntegral n

