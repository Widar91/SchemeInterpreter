module Environment where

import Grammar

import Data.Maybe
import Data.IORef
import Control.Monad.Except

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOErrorM LispVal
getVar envRef var  =  do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOErrorM LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value   

defineVar :: Env -> String -> LispVal -> IOErrorM LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where 
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = liftM ((,) var) (newIORef value)

