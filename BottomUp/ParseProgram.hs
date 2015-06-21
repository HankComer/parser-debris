module ParseProgram where
import CommonData
import ParseDecl
import Reformat
import Tokenize




doEverything :: String -> ([RealDecl], [String])
doEverything str = case tokensPrecsImports str of
  ([], _, _) -> error "syntax error, didn't make tokens"
  (code, precs, imports) -> (map (reDeclare precs) (preDecl code), imports)



reDeclare :: [Prec] -> PreDecl -> RealDecl
reDeclare precs (FuncDec name args inter) = Decl name args (translate parsePat $ delve precs inter)
reDeclare precs (OpDec name arg1 arg2 inter) = Decl name [arg1, arg2] (translate parsePat $ delve precs inter)