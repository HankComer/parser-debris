module StdLib where
import CommonData
import Control.Monad (join)
import Utils
import EvalExpr (compress)



stdEnv :: Env
stdEnv = Env [
  ("mapIO", mapIO),
  (">>=", bindIO),
  (">>", chainIO),
  ("getLine", IO (fmap StringV getLine)),
  ("putStrLn", Lam $ with (\(StringV a) -> IO (putStrLn a >> return UnitV))),
  ("strCat", strCat),
  ("primEq", primEq),
  ("headStr", headStr),
  ("tailStr", tailStr),
  ("toStr", toStr),
  ("catch", catch),
  ("throw", throw)]


mapIO :: Value
mapIO = Lam (\a -> Lam (\b -> IO (fmap (unLam a) (unIO b))))

chainIO :: Value
chainIO = Lam (\a -> Lam (\b -> IO (unIO a >> unIO b)))


bindIO = Lam (\a -> Lam (\b -> IO $ do
    blah <- unIO a
    unIO (unLam b a)))

strCat :: Value
strCat = Lam (\a -> Lam (\b -> StringV (unString a ++ unString b)))

primEq' :: Value -> Value -> Int
primEq' a b = if a == b then 1 else 0

primEq :: Value
primEq = Lam $ with (\a -> Lam $ with (\b -> IntV (primEq' a b)))

headStr = Lam (\a -> StringV [head $ unString a])

tailStr = Lam (\a -> StringV (tail $ unString a))


toStr = Lam $ with (\a -> StringV $ show a)

catch = Lam $ Lam . unError

throw = Lam Error