module TheFunctions where
import EvalExpr
import Simplicity



takeString :: (String -> Expression a) -> CValue -> Expression a
takeString f (String a) = f a
takeString f _ = crash TypeError

oneArg :: (CValue -> Expression a) -> ([CValue] -> Expression a)
oneArg f [a] = f a
oneArg f a = crash (ArgError $ length a)

transformIO1 :: (a -> IO b) -> (a -> Expression b)
transformIO1 = fmap transformIO

transformIO2 :: (a -> b -> IO c) -> (a -> b -> Expression c)
transformIO2 = fmap transformIO1

void :: (a -> Expression b) -> (a -> Expression CValue)
void f arg = do
  f arg
  return (Whole 1)



stdEnv' :: [(String, [CValue] -> Expression CValue)]
stdEnv' = [
    ("putStr", oneArg . takeString $ void (transformIO1 putStr)),
    ("getLine", const $ transformIO (fmap String getLine)),
    ("print", oneArg $ void (transformIO1 print))]

stdEnv = Env ((fmap fmap fmap Func) stdEnv', Effect makeVoid)


ident = Single . Ident

str = Single . Str

int = Single . Int

myCrap = sequence_ $ map evaluate [
  Infix "=" (ident "line") (Call (ident "getLine")),
  CallArgs (ident "putStr") [str "hello, world!\n"],
  CallArgs (ident "putStr") [ident "line"],
  CallArgs (ident "putStr") [str "hello, world!\n"]]

indexBug = sequence_ $ map evaluate [
  Infix "=" (ident "arr") (ArrayLit [ArrayLit [int 0, int 1], ArrayLit [int 5, int 4], int 3]),
  Infix "=" (ident "x") (int 0),
  Infix "=" (Index (Index (ident "arr") (Postfix "++" (ident "x"))) (int 0)) (str "hi"),
  CallArgs (ident "print") [ident "x"],
  CallArgs (ident "print") [(ident "arr")]]
  
  