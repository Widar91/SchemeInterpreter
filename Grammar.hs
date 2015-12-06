module Grammar where

import Numeric
import Data.Complex
import Data.Ratio
import Data.Array
import Data.IORef
import Text.Parsec.Error
import Control.Monad.Except
import System.IO


type Env       = IORef [(String, IORef LispVal)]
type ErrorM    = Either LispError
type IOErrorM  = ExceptT LispError IO

data LispVal   = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | Float Double
               | Ratio Rational
               | Complex (Complex Double)
               | Char Char
               | String String
               | Bool Bool
               | Vector (Array Int LispVal)
               | Port Handle
               | PrimitiveFunc ([LispVal] -> ErrorM LispVal)
               | IOFunc ([LispVal] -> IOErrorM LispVal)
               | Func { params  :: [String]
                      , vararg  :: Maybe String
                      , body    :: [LispVal] 
                      , closure :: Env }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal   where show = showVal
instance Show LispError where show = showError


showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _)               = "<IO port>"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (IOFunc _)             = "<IO primitive>"
showVal (Func { params  = args
              , vararg  = vargs
              , body    = body
              , closure = env     
              })               = "(lambda (" ++ unwords (map show args)
                              ++ maybe "" (" . " ++) vargs
                              ++ ") ...)"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default err)                 = "Error: " ++ err

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


-----------------------------------
-- IOErrorM transformer functions
-----------------------------------

liftError :: ErrorM a -> IOErrorM a
liftError (Left err)  = throwError err
liftError (Right val) = return val

runIOError :: IOErrorM String -> IO String
runIOError a = liftM extractValue (runExceptT (catchError a (return . show)))
  where
    extractValue (Right v) = v





