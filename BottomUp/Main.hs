module Main where
import Reformat
import EvalExpr
import ConvertProgram
import ParseProgram
import CommonData
import StdLib

getEnv :: Env -> String -> Env
getEnv env str = makeGlobals env (doEverything str)

importAndRun :: Env -> String -> Value
importAndRun blah str = eval (getEnv blah str) (Env []) (Atom (Id "main"))

runMain :: String -> Value
runMain str = eval (getEnv (Env []) str) (Env []) (Atom (Id "main"))