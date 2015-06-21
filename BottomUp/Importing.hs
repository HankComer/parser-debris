module Importing where
import ParserType
import Control.Applicative
import Tokenize
import ConvertProgram
import ParseProgram
import CommonData
import StdLib
import EvalExpr (rewrite)
import Data.Char (isSpace)



getEnvImports :: Env -> String -> (Env, [String])
getEnvImports env str = case doEverything str of
  (decls, imports) -> (makeGlobals rewrite env decls, imports)


data ImportMode = User | Builtin


getBuiltin :: Parser (String, ImportMode)
getBuiltin = do
    many (sat isSpace)
    char '<'
    name <- some (sat (/= '>'))
    char '>'
    return (name, Builtin)





simpleImport str = case (terminal getBuiltin) str of
    Just a -> a

loadFile :: String -> IO Env
loadFile fname = do
   text <- readFile fname
   getEnv text


getEnv :: String -> IO Env
getEnv text = do
   let (decls, imports) = doEverything text
   envs <- mapM (importSimple . simpleImport) imports
   let extEnv = foldr1 squish envs
   return (makeGlobals rewrite extEnv decls)
   
   

importSimple (blah, User) = do
    loadFile (blah ++ ".wg")
importSimple (blah, Builtin) = case lookup blah libs of
    Just a -> return a
    Nothing -> importSimple (blah, User)
   


libs = [
  ("StdEnv", stdEnv)]