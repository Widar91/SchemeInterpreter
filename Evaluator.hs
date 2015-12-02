{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Grammar

import Control.Monad.Except


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ErrorMonad a)


eval :: LispVal -> ErrorMonad LispVal
eval val@(String _)                = return val
eval val@(Number _)                = return val
eval val@(Bool _)                  = return val
eval (List [Atom "quote", val])    = return val
eval (List [Atom "if", p, e1, e2]) = do 
    result <- eval p
    case result of
        Bool False -> eval e2
        Bool True  -> eval e1
        badType    -> throwError $ TypeMismatch "bool" badType
eval val@(List [Atom "cond", List cs]) = cond cs
  where
    cond [List (Atom "else" : exps)] = evaluateExprs exps 
    cond (List (test : exps) : cs) = do 
        result <- eval test 
        case result of 
            Bool False -> if null cs 
                          then throwError $ BadSpecialForm "No true clauses in cond expression" val
                          else cond cs
            Bool True  -> evaluateExprs exps
            badType    -> throwError $ TypeMismatch "bool" badType
    cond [] = throwError $ BadSpecialForm "Missing clauses in cond expression" val                
    cond _  = throwError $ BadSpecialForm "Unexpected clause form in cond expression" val
    evaluateExprs exps = foldl (\_ v -> eval v) (return $ Bool True) exps
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval unrecognizedForm = throwError $ BadSpecialForm "Unrecognized form during evaluation" unrecognizedForm

apply :: String -> [LispVal] -> ErrorMonad LispVal
apply func args = maybe 
    (throwError $ NotFunction "Unrecognized primitive function args" func) 
    ($ args)  
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ErrorMonad LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("boolean?", unaryOp isBoolean),
              ("symbol?", unaryOp isSymbol),
              ("char?", unaryOp isChar),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("vector?", unaryOp isVector),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ErrorMonad LispVal
numericBinOp op []     = throwError $ NumArgs 2 []
numericBinOp op p@[_]  = throwError $ NumArgs 2 p
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op 

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ErrorMonad a) -> (a -> a -> Bool) -> [LispVal] -> ErrorMonad LispVal
boolBinop unpacker op args = if length args /= 2 
    then throwError $ NumArgs 2 args
    else do  
        left  <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

unpackNum :: LispVal -> ErrorMonad Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
    if null parsed 
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ErrorMonad String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ErrorMonad Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ErrorMonad LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ErrorMonad LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ErrorMonad LispVal
cons [x, List []]         = return $ List [x]
cons [x, List xs]         = return $ List $ x : xs
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y]               = return $ DottedList [x] y
cons badArgList           = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ErrorMonad LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = eqLists arg1 arg2 eqv
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ErrorMonad LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List arg1), (List arg2)] = eqLists arg1 arg2 equal
equal [arg1, arg2] = do 
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqLists :: [LispVal] -> [LispVal] -> ([LispVal] -> ErrorMonad LispVal) -> ErrorMonad LispVal
eqLists xs ys eq = return $ Bool $ (length xs == length ys) && (all eqvPair $ zip xs ys)
  where 
    eqvPair (x, y) = case eq [x, y] of
        Left err         -> False
        Right (Bool val) -> val

unpackEquals :: LispVal -> LispVal -> Unpacker -> ErrorMonad Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do  unpacked1 <- unpacker arg1
                                                    unpacked2 <- unpacker arg2
                                                    return $ unpacked1 == unpacked2 
                                                `catchError` (const $ return False)

unaryOp :: (LispVal -> ErrorMonad LispVal) -> [LispVal] -> ErrorMonad LispVal
unaryOp f [v]  = f v
unaryOp f vals = throwError $ NumArgs 1 vals

isBoolean, isSymbol, isChar, isString, isNumber, isVector, isList :: LispVal -> ErrorMonad LispVal
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

symbol2string :: LispVal -> ErrorMonad LispVal
symbol2string (Atom v) = return $ String v
symbol2string badArg   = throwError $ TypeMismatch "symbol" badArg

string2symbol :: LispVal -> ErrorMonad LispVal
string2symbol (String s) = return $ Atom s
string2symbol badArg     = throwError $ TypeMismatch "string" badArg
