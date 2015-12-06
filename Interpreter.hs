module Main where

import Grammar
import Parser
import Evaluator
import Environment

import System.Environment
import System.IO
import Control.Monad.Except
import Data.IORef


main :: IO ()
main = do 
    args <- getArgs
    if null args then runRepl else runProgram args

runRepl :: IO ()
runRepl = do 
    env <- primitiveBindings 
    evalAndPrint env "(load \"lib/stdlib.scm\")" 
    replLoop (readPrompt "Lisp >>= ") . evalAndPrint $ env

runProgram :: [String] -> IO ()
runProgram args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    runIOError (liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars ps
  where 
    ps = (map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) primitivesIO)
    makeFunc constructor (var, func) = (var, constructor func)

replLoop :: Monad m => m String -> (String -> m ()) -> m ()
replLoop prompt action = do 
    result <- prompt
    case result of
        ":q"      -> return ()
        ":std"    -> action "(load \"lib/stdlib.scm\")" >> replLoop prompt action
        otherwise -> action result >> replLoop prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOError $ liftM show $ liftError (readExpr expr) >>= eval env
  where
    extractValue (Right v) = v


{-
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action
-}







