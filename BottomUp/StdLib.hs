module StdLib where
import CommonData
import Control.Monad (join)
import Utils



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


bindIO = Lam $ with (\(IO a) -> Lam $ with (\(Lam f) -> IO $ do
    (IO r) <- fmap f a
    r))