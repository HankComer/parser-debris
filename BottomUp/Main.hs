module Main where
import Reformat
import EvalExpr
import ConvertProgram
import ParseProgram
import CommonData
import StdLib
import Importing







loadAndRun :: String -> IO Value
loadAndRun fname = do
    env <- loadFile fname
    return $ rewrite env (Env []) (Atom (Id "main"))
    

main = do
  val <- loadAndRun "main.wg"
  print val

checkSyntax :: String -> IO ()
checkSyntax fname = do
    (Env env) <- loadFile fname
    putStrLn $ "Loaded '" ++ fname ++ "'."
    putStrLn "Defined functions: "
    mapM_ (putStrLn . fst) env