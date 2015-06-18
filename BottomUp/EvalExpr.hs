module EvalExpr where
import CommonData
import Control.Monad (zipWithM)



squish :: Env -> Env -> Env
squish (Env a) (Env b) = Env $ b ++ a

resolve :: Env -> Env -> String -> Value
resolve (Env globals) (Env locals) str = case lookup str locals of
    Just a -> a
    Nothing -> case lookup str globals of
      Just a -> a
      Nothing -> error $ show str ++ " is out of scope"


eval :: Env -> Env -> ParseTree -> Value
eval _ _ (Atom (Int a)) = IntV a
eval _ _ (Atom (Double a)) = DoubleV a
eval _ _ (Atom (String a)) = StringV a
eval globals locals (Atom (Id a)) = Thunk (\l -> resolve globals l a) locals
--eval globals locals (Abs (UnQuote pat) body) = Lam (\arg -> Thunk (\l -> eval globals (squish l (match pat (deepEval arg))) body) locals)
eval globals locals (Abs pat body) = Lam (\arg -> Thunk (\l -> eval globals (squish l (match pat arg)) body) locals)
eval globals locals (Apply a b) = Thunk (\l -> apply (eval globals l a) (eval globals l b)) locals
eval globals locals (Case asdf' things) =
 let
  asdf = eval globals locals asdf'
  blah :: [(Pattern, ParseTree)]
  blah = map (\(Abs a b) -> (a, b)) things
  thing :: [(Pattern, ParseTree)] -> Value
  thing [] = error "Pattern match failure in case expression"
  thing ((pat, body):rest) = case match' pat asdf of
    Just env -> Thunk (\locals' -> eval globals (squish locals' env) body) locals 
    Nothing -> thing rest
  in thing blah
eval globals locals Unit = UnitV
eval globals locals (Tuple blah) = TupleV (map (eval globals locals) blah)

apply (Lam a) = a
apply (Thunk f a) = apply (f a)

deepEval :: Value -> Value
deepEval (Thunk f locals) = deepEval $ f locals
deepEval (TupleV things) = TupleV $ fmap deepEval things
deepEval a = a

oneLayer :: Value -> Value
oneLayer (Thunk f l) = f l
oneLayer a = a

match :: Pattern -> Value -> Env
match blah foo = case match' blah foo of
    Just a -> a
    Nothing -> error "Pattern match failure"

match' :: Pattern -> Value -> Maybe Env
match' (UnQuote a) b = match' a (deepEval b)
match' (VarP str) a = Just (Env [(str, a)])
match' a (Thunk f x) = match' a (f x)
match' (TupleP a) (TupleV b) = if length a == length b then (fmap (foldr1 squish) $ zipWithM match' a b) else Nothing
match' (LitP (Double a)) (DoubleV b) = if a == b then Just (Env []) else Nothing
match' (LitP (Int a)) (IntV b) = if a == b then Just (Env []) else Nothing
match' (LitP (String a)) (StringV b) = if a == b then Just (Env []) else Nothing
match' _ _ = Nothing

matchCase :: [Pattern] -> Value -> Env
matchCase [] _ = error "pattern match failure"
matchCase (a:rest) val = case match' a val of
    Just b -> b
    Nothing -> matchCase rest val




  