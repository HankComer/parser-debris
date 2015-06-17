module EvalExpr where
import CommonData



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
eval globals locals (Abs (UnQuote pat) body) = Lam (\arg -> Thunk (\l -> eval globals (squish l (match pat (deepEval arg))) body) locals)
eval globals locals (Abs pat body) = Lam (\arg -> Thunk (\l -> eval globals (squish l (match pat arg)) body) locals)
eval globals locals (Apply a b) = Thunk (\l -> apply (eval globals l a) (eval globals l b)) locals


apply (Lam a) = a
apply (Thunk f a) = apply (f a)

deepEval :: Value -> Value
deepEval (Thunk f locals) = deepEval $ f locals
deepEval (TupleV things) = TupleV $ fmap deepEval things
deepEval a = a




match :: Pattern -> Value -> Env
match = undefined