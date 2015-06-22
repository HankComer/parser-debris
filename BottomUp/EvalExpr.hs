module EvalExpr where
import CommonData
import Control.Monad (zipWithM)
import ConvertProgram (makeGlobals)





resolve :: Env -> Env -> String -> Value
resolve (Env globals) (Env locals) str = case lookup str locals of
    Just a -> a
    Nothing -> case lookup str globals of
      Just a -> a
      Nothing -> error $ "Woogy failed: " ++ show str ++ " is out of scope"


eval :: Env -> Env -> ParseTree -> Value
eval = rewrite


rewrite :: Env -> Env -> ParseTree -> Value
rewrite _ _ (Atom (Int a)) = IntV a
rewrite _ _ (Atom (Double a)) = DoubleV a
rewrite _ _ (Atom (String a)) = StringV a
rewrite globals locals (Atom (Id a)) = Thunk (\l -> resolve globals l a) locals
rewrite globals locals (Atom (Op a)) = resolve globals locals a
rewrite globals locals (Abs pat body) = Thunk (\locals' -> Lam (\arg -> Thunk (\l -> rewrite globals (squish l (match pat arg)) body) locals')) locals
rewrite globals locals (Apply a b) = Thunk (\l -> apply (rewrite globals l a) (rewrite globals l b)) locals
rewrite globals locals (Case asdf' things) =
 let
  asdf = rewrite globals locals asdf'
  blah :: [(Pattern, ParseTree)]
  blah = things
  thing :: [(Pattern, ParseTree)] -> Env -> Value
  thing [] l = Error $ TupleV [StringV "casefail", StringV $ "Pattern match failure in case expression: " ++ unlines (fmap (show . fst) blah) ++ "\nThe value: " ++ show asdf]
  thing ((pat, body):rest) l = case match' pat asdf of
    Just env -> rewrite globals (squish l env) body
    Nothing -> thing rest l
  in Thunk (thing blah) locals
rewrite globals locals Unit = UnitV
rewrite globals locals (Tuple [blah]) = rewrite globals locals blah
rewrite globals locals (Tuple blah) = Thunk (\l -> TupleV $ map (rewrite globals l) blah) locals
rewrite globals locals (Let clauses res) = 
 let
  things = fmap (\(a, b, c) -> Decl a b c) clauses
 in rewrite globals (squish locals (makeGlobals rewrite (squish locals globals) things)) res
rewrite globals locals crap = error $ "Can't rewrite " ++ show crap

apply (Lam a) = a
apply (Thunk f a) = apply (f a)

deepEval :: Value -> Value
deepEval (Thunk f locals) = deepEval $ f locals
deepEval (TupleV things) = let {foo = deepList things} in foo `seq` TupleV foo
deepEval a = a

compress (Thunk f a) = compress $ f a
compress a = a

deepList [] = []
deepList (a:rest) = let {foo = deepEval a} in foo `seq` foo : deepList rest

oneLayer :: Value -> Value
oneLayer (Thunk f l) = f l
oneLayer a = a

match :: Pattern -> Value -> Env
match blah foo = case match' blah foo of
    Just a -> a
    Nothing -> error $ "Pattern match failure: " ++ show blah ++ " AND " ++ show foo

match' :: Pattern -> Value -> Maybe Env
match' (VarP ('_':_)) a = Just (Env [])
match' (UnQuote a) b = match' a (deepEval b)
match' (VarP str) a = Just (Env [(str, a)])
match' a (Thunk f x) = match' a (f x)
match' (TupleP [a]) b = match' a b
match' (TupleP a) (TupleV b) = if length a == length b then if a == [] then (Just $ Env []) else (fmap (foldr1 squish) $ zipWithM match' a b) else Nothing
match' (LitP (Double a)) (DoubleV b) = if a == b then Just (Env []) else Nothing
match' (LitP (Int a)) (IntV b) = if a == b then Just (Env []) else Nothing
match' (LitP (String a)) (StringV b) = if a == b then Just (Env []) else Nothing
match' UnitP UnitV = Just (Env [])
match' _ _ = Nothing

matchCase :: [Pattern] -> Value -> Env
matchCase [] _ = error "pattern match failure"
matchCase (a:rest) val = case match' a val of
    Just b -> b
    Nothing -> matchCase rest val




  