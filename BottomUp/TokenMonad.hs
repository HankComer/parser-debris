module TokenMonad where
import Control.Applicative
import Control.Monad (ap)




newtype Consumer a b = Consumer {consume :: ([a] -> [(b, [a])])}