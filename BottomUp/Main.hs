module Main where
import Reformat
import EvalExpr
import ConvertProgram
import ParseProgram
import CommonData

getEnv :: String -> Env
getEnv str = makeGlobals (doEverything str)

runMain :: String -> Value
runMain str = eval (getEnv str) (Env []) (Atom (Id "main"))