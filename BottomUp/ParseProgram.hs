module ParseProgram where
import CommonData
import ParseDecl
import Reformat
import Tokenize




doEverything :: String -> [RealDecl]
doEverything str = case tokensAndPrecs str of
  (code, precs) -> map (reDeclare precs) (preDecl code)



reDeclare :: [Prec] -> PreDecl -> RealDecl
reDeclare precs (FuncDec name args inter) = Decl name args (translate parsePat $ delve precs inter)
reDeclare precs (OpDec name arg1 arg2 inter) = Decl name [arg1, arg2] (translate parsePat $ delve precs inter)