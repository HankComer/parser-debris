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
  ("putStrLn", Lam (\(StringV a) -> IO (putStrLn a >> return UnitV)))]


mapIO :: Value
mapIO = Lam (\(Lam f) -> Lam (\(IO a) -> IO (fmap f a)))

chainIO :: Value
chainIO = Lam (\(IO a) -> Lam (\(IO b) -> IO (a >> b)))


bindIO = Lam $ with (\(IO a) -> Lam $ with (\(Lam f) -> IO $ do
    (IO r) <- fmap f a
    r))

