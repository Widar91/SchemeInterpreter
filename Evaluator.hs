{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Grammar
import Parser
import Environment

import Data.Maybe
import System.IO
import Control.Monad.Except


eval :: Env -> LispVal -> IOErrorM LispVal
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env (Atom id)                  = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", p, e1, e2]) = do 
    result <- eval env p
    case result of
        Bool False -> eval env e2
        Bool True  -> eval env e1
        badType    -> throwError $ TypeMismatch "bool" badType
eval env val@(List (Atom "cond" : cs)) = 
    cond cs
      where
        cond [List (Atom "else" : exps)] = evaluateExprs env exps 
        cond (List (test : exps) : cs) = do 
            result <- eval env test 
            case result of 
                Bool False -> if null cs 
                              then throwError $ BadSpecialForm "No true clauses in cond expression" val
                              else cond cs
                Bool True  -> evaluateExprs env exps
                badType    -> throwError $ TypeMismatch "bool" badType
        cond [] = throwError $ BadSpecialForm "Missing clauses in cond expression" val                
        cond _  = throwError $ BadSpecialForm "Unexpected clause form in cond expression" val
        evaluateExprs env [e]    = eval env e
        evaluateExprs env (e:es) = eval env e >> evaluateExprs env es
        evaluateExprs env []     = return $ Bool True
eval env (List [Atom "set!", Atom var, e])   = 
    eval env e >>= setVar env var
eval env (List [Atom "define", Atom var, e]) = 
    eval env e >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    f  <- eval env function 
    as <- mapM (eval env) args
    apply f as
eval env unrecognizedForm = 
    throwError $ BadSpecialForm "Unrecognized form during evaluation" unrecognizedForm

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOErrorM LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs    = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOErrorM LispVal
apply (IOFunc f) args = f args
apply (PrimitiveFunc f) args = liftError $ f args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where 
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing      -> return env


---------------------------------        
-- Primitive Language Functions  
---------------------------------

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ErrorM a)

primitives :: [(String, [LispVal] -> ErrorM LispVal)]
primitives =  
    [ ("+", numericBinOp (+))
    , ("-", numericBinOp (-))
    , ("*", numericBinOp (*))
    , ("/", numericBinOp div)
    , ("mod", numericBinOp mod)
    , ("quotient", numericBinOp quot)
    , ("remainder", numericBinOp rem)
    , ("boolean?", unaryOp isBoolean)
    , ("symbol?", unaryOp isSymbol)
    , ("char?", unaryOp isChar)
    , ("string?", unaryOp isString)
    , ("number?", unaryOp isNumber)
    , ("vector?", unaryOp isVector)
    , ("symbol->string", unaryOp symbol2string)
    , ("string->symbol", unaryOp string2symbol)
    , ("=", numBoolBinop (==))
    , ("<", numBoolBinop (<))
    , (">", numBoolBinop (>))
    , ("/=", numBoolBinop (/=))
    , (">=", numBoolBinop (>=))
    , ("<=", numBoolBinop (<=))
    , ("&&", boolBoolBinop (&&))
    , ("||", boolBoolBinop (||))
    , ("string=?", strBoolBinop (==))
    , ("string<?", strBoolBinop (<))
    , ("string>?", strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]

primitivesIO :: [(String, [LispVal] -> IOErrorM LispVal)]
primitivesIO = 
    [ ("apply", applyProc)
    , ("open-input-file", makePort ReadMode)
    , ("open-output-file", makePort WriteMode)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read", readProc)
    , ("write", writeProc)
    , ("read-contents", readContents)
    , ("read-all", readAll)
    ]


---------------
-- Primitives
---------------

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ErrorM LispVal
numericBinOp op []     = throwError $ NumArgs 2 []
numericBinOp op p@[_]  = throwError $ NumArgs 2 p
numericBinOp op params = liftM (Number . foldl1 op) (mapM unpackNum params)

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ErrorM a) -> (a -> a -> Bool) -> [LispVal] -> ErrorM LispVal
boolBinop unpacker op args = if length args /= 2 
    then throwError $ NumArgs 2 args
    else do  
        left  <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

car :: [LispVal] -> ErrorM LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ErrorM LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ErrorM LispVal
cons [x, List []]         = return $ List [x]
cons [x, List xs]         = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y]               = return $ DottedList [x] y
cons badArgList           = throwError $ NumArgs 2 (String "cons" : badArgList)

eqv :: [LispVal] -> ErrorM LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2]         = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2]         = return $ Bool (arg1 == arg2)
eqv [Atom arg1, Atom arg2]             = return $ Bool (arg1 == arg2)
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = eqLists arg1 arg2 eqv
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ErrorM LispVal
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List arg1, List arg2] = eqLists arg1 arg2 equal
equal [arg1, arg2] = do 
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) unpackers
    eqvEquals       <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
      where
        unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList

eqLists :: [LispVal] -> [LispVal] -> ([LispVal] -> ErrorM LispVal) -> ErrorM LispVal
eqLists xs ys eq = return $ Bool $ (length xs == length ys) && all eqvPair (zip xs ys)
  where 
    eqvPair (x, y) = case eq [x, y] of
        Left err         -> False
        Right (Bool val) -> val

unpackEquals :: LispVal -> LispVal -> Unpacker -> ErrorM Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do  
      unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2 
    `catchError` const (return False)

unpackNum :: LispVal -> ErrorM Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
    if null parsed 
    then throwError $ TypeMismatch "number" (String n)
    else return . fst . head $ parsed
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ErrorM String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ErrorM Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unaryOp :: (LispVal -> ErrorM LispVal) -> [LispVal] -> ErrorM LispVal
unaryOp f [v]  = f v
unaryOp f vals = throwError $ NumArgs 1 vals

isBoolean, isSymbol, isChar, isString, isNumber, isVector, isList :: LispVal -> ErrorM LispVal
isBoolean (Bool _)      = return $ Bool True
isBoolean _             = return $ Bool False
isSymbol (Atom _)       = return $ Bool True
isSymbol _              = return $ Bool False
isChar (Char _)         = return $ Bool True
isChar _                = return $ Bool False
isString (String _)     = return $ Bool True
isString _              = return $ Bool False
isNumber (Number _)     = return $ Bool True
isNumber _              = return $ Bool False
isVector (Vector _)     = return $ Bool True
isVector _              = return $ Bool False
isList (List _)         = return $ Bool True
isList (DottedList _ _) = return $ Bool True
isList _                = return $ Bool False

symbol2string :: LispVal -> ErrorM LispVal
symbol2string (Atom v) = return $ String v
symbol2string badArg   = throwError $ TypeMismatch "symbol" badArg

string2symbol :: LispVal -> ErrorM LispVal
string2symbol (String s) = return $ Atom s
string2symbol badArg     = throwError $ TypeMismatch "string" badArg


------------------
-- IO Primitives
------------------

applyProc :: [LispVal] -> IOErrorM LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOErrorM LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOErrorM LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOErrorM LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftError . readExpr

writeProc :: [LispVal] -> IOErrorM LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOErrorM LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOErrorM [LispVal]
load filename = liftIO (readFile filename) >>= liftError . readExprList

readAll :: [LispVal] -> IOErrorM LispVal
readAll [String filename] = liftM List $ load filename