module Importing where
import ParserType
import Control.Applicative
import Tokenize
import ConvertProgram
import ParseProgram
import CommonData
import StdLib

getEnv :: Env -> String -> Env
getEnv env str = makeGlobals env (doEverything str)


data ImportMode = User | Builtin


getBuiltin :: Parser (String, ImportMode)
getBuiltin = do
    space
    char '<'
    name <- some (sat (/= '>'))
    char '>'
    space
    return (name, Builtin)

getUserMade :: Parser (String, ImportMode)
getUserMade = do
    space
    char '"'
    name <- some (sat (/= '"'))
    char '"'
    space
    return (name, User)

simpleImport str = case (terminal $ getUserMade <|> getBuiltin) str of
    Just a -> a

loadFile :: String -> IO (String, Env)
loadFile fname = do
    text <- readFile fname
    let (imports, stuff) = removeImports text
    envs <- mapM (importSimple . simpleImport) imports
    case envs of
        [] -> return (stuff, Env [])
        [a] -> return (stuff, a)
        a -> return (stuff, foldr1 squish a)

importSimple (blah, User) = do
    (str, env) <- loadFile (blah ++ ".wg")
    return (getEnv env str)
importSimple (blah, Builtin) = case lookup blah libs of
    Just a -> return a
    Nothing -> error $ "couldn't find library '" ++ blah ++ "'"
   


libs = [
  ("StdEnv", stdEnv)]