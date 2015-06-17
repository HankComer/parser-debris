module ParseProgram where
import CommonData
import ParseDecl
import Reformat
import Tokenize




doEverything :: String -> [RealDecl]
doEverything str = case tokensAndPrecs str of
  (code, precs) -> map (reDeclare precs) (preDecl code)



reDeclare :: [Prec] -> PreDecl -> RealDecl
reDeclare precs (FuncDec name args toks) = Decl name args (doItAll precs toks)
reDeclare precs (OpDec name arg1 arg2 toks) = Decl name [arg1, arg2] (doItAll precs toks)