module Main where
import Reformat
import EvalExpr
import ConvertProgram
import ParseProgram
import CommonData
import StdLib
import Importing
import System.IO.Unsafe



importAndRun :: Env -> String -> Value
importAndRun blah str = eval (getEnv blah str) (Env []) (Atom (Id "main"))

runMain :: String -> Value
runMain str = eval (getEnv (Env []) str) (Env []) (Atom (Id "main"))

loadAndRun :: String -> IO Value
loadAndRun fname = do
    (text, env) <- loadFile fname
    return $ importAndRun env text
    