module EvalExpr where
import Simplicity
import Control.Applicative
import Control.Monad (ap)

data CValue
  = String String
  | Whole Int
  | Real Double
  | Compound [(String, CValue)]
  | Array [CValue]
  | Func ([CValue] -> Expression CValue)

data LValue
  = Name String

toLValue :: Expr -> LValue
toLValue = undefined

newtype Environment = Env ([(String, CValue)], Effect)


newtype Void = Void Void

newtype Effect = Effect Void



makeVoid :: Void
makeVoid = undefined


assign :: LValue -> CValue -> Expression CValue
assign (Name name) val = Expr (\(Env (e, v)) -> Right (val, Env ((name, val):e, v)))

resolve :: String -> Expression CValue
resolve name = Expr (\(Env (e, v)) -> case lookup name e of
   Just thing -> Right (thing, Env (e, v))
   Nothing -> Left (NotInScope name))

crash :: Failure -> Expression a
crash thing = Expr (const (Left thing))

data Failure = Halt | NotInScope String | TypeError | NoMember String | DivideZero

newtype Expression a = Expr {eval :: (Environment -> Either Failure (a, Environment))}

instance Functor Expression where
  fmap f (Expr a) = Expr (\e -> fmap (\(b, c) -> (f b, c)) (a e))

instance Applicative Expression where
  pure = return
  (<*>) = ap

instance Alternative Expression where
  (Expr a) <|> (Expr b) = undefined
  empty = Expr (\e -> Left Halt)

instance Alternative (Either a) where
  (Right a) <|> _ = Right a
  (Left _) <|> (Right a) = Right a
  (Left _) <|> (Left a) = Left a
  empty = Left (error "crash")

instance Monad Expression where
  return x = Expr (\e -> Right (x, e))
  (Expr a) >>= f = Expr (\e -> case a e of
    Right (x, y) -> case eval (f x) y of
      Right (r, e') -> Right (r, e')
      Left z -> Left z
    Left z -> Left z)

incdec :: String -> CValue -> Expression CValue
incdec "++" (Whole a) = return (Whole $ succ a)
incdec "++" (Real a) = return (Real $ succ a)
incdec "--" (Whole a) = return (Whole $ pred a)
incdec "--" (Real a) = return (Real $ pred a)
incdec _ _ = crash TypeError

negative :: CValue -> Expression CValue
negative (Whole a) = return (Whole $ negate a)
negative (Real a) = return (Real $ negate a)
negative _ = crash TypeError

cNot :: CValue -> Expression CValue
cNot (Whole 0) = return (Whole 1)
cNot (Whole _) = return (Whole 0)
cNot _ = crash TypeError

call :: CValue -> [CValue] -> Expression CValue
call (Func f) args = Expr (\(Env (e, v)) -> case eval (f args) (Env (e, v)) of
    Right (a, Env (e', v')) -> Right (a, Env (e, v'))
    Left a -> Left a)
call _ _ = crash TypeError

memberAccess :: CValue -> String -> Expression CValue
memberAccess (Compound a) v = case lookup v a of
  Just r -> return r
  Nothing -> crash $ NoMember v
memberAccess _ _ = crash TypeError

arrayIndex :: CValue -> CValue -> Expression CValue
arrayIndex (Array a) (Whole b) = return (a !! b)
arrayIndex (String a) (Whole b) = return ([a !! b])
arrayIndex _ _ = crash TypeError

cTimes :: CValue -> CValue -> Expression CValue
cTimes (Whole a) (Whole b) = return (Whole $ a * b)
cTimes (Whole a) (Real b) = return (Real $ fromIntegral a * b)
cTimes (Real a) (Whole b) = return (Real $ a * fromIntegral b)
cTimes (Real a) (Real b) = return (Real $ a * b)
cTimes _ _ = crash TypeError

cDivide _ (Whole 0) = crash DivideZero
cDivide _ (Real 0) = crash DivideZero
cDivide (Whole a) (Whole b) = return (Whole $ a `div` b)
cDivide (Whole a) (Real b) = return (Real $ fromIntegral a / b)
cDivide (Real a) (Whole b) = return (Real $ a / fromIntegral b)
cDivide (Real a) (Real b) = return (Real $ a / b)
cDivide _ _ = crash TypeError

    

evaluate :: Expr -> Expression CValue
evaluate (Single (Int a)) = return (Whole a)
evaluate (Single (Doub a)) = return (Real a)
evaluate (Single (Str a)) = return (String a)
evaluate (Single (Ident a)) = resolve a
evaluate (Prefix "++" a) = do
  thing <- evaluate a
  blah <- incdec "++" thing
  assign (toLValue a) blah
  return blah
evaluate (Prefix "--" a) = do
  thing <- evaluate a
  blah <- incdec "--" thing
  assign (toLValue a) blah
  return blah
evaluate (Prefix "-" a) = do
  thing <- evaluate a
  negative thing
evaluate (Prefix "!" a) = do
  thing <- evaluate a
  cNot thing
evaluate (Postfix "++" a) = do
  thing <- evaluate a
  blah <- incdec "++" thing
  assign (toLValue a) blah
  return thing
evaluate (Postfix "--" a) = do
  thing <- evaluate a
  blah <- incdec "--" thing
  assign (toLValue a) blah
  return thing
evaluate (CallArgs f args) = do
  func <- evaluate f
  args' <- mapM evaluate args
  call func args'
evaluate (Member s m) = do
  thing <- evaluate s
  memberAccess thing m
evaluate (Call f) = do
  func <- evaluate f
  call func []
evaluate (Index a b) = do
  arr <- evaluate a
  ind <- evaluate b
  arrayIndex arr ind
evaluate (Infix "*" a b) = do
  x <- evaluate a
  y <- evaluate b
  cTimes x y
evaluate (Infix "/" a b) = do
  x <- evaluate a
  y <- evaluate b
  cDivide x y

  
  
  