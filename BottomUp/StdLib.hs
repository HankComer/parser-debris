module StdLib where
import CommonData


mapIO :: Value
mapIO = Lam (\(Lam f) -> Lam (\(IO a) -> IO (fmap f a)))