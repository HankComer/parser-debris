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
  ("putStrLn", Lam $ with (\(StringV a) -> IO (putStrLn a >> return UnitV)))]


mapIO :: Value
mapIO = Lam $ with (\(Lam f) -> Lam $ with (\(IO a) -> IO (fmap f a)))

chainIO :: Value
chainIO = Lam $ with (\(IO a) -> Lam $ with (\(IO b) -> IO (a >> b)))


bindIO = Lam $ with (\(IO a) -> Lam $ with (\x -> case x of
  (Lam f) -> IO $ do
    r <- fmap (with f) a
    case compress r of
        IO dingus -> dingus
        notIO -> error $ "bindIO's do again " ++ show notIO
  blah -> error $ "bindIO again " ++ show blah))