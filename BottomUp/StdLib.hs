module StdLib where
import CommonData
import Control.Monad (join)




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


bindIO = Lam (\(IO a) -> Lam (\(Lam f) -> IO $ do
    (IO dingus) <- fmap f a
    dingus))

