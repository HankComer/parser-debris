module TokenMonad where
import Control.Applicative
import Control.Monad (ap)




newtype Consumer a b = Consumer {consume :: ([a] -> [(b, [a])])}

item :: Consumer a a
item = Consumer (\s -> case s of
    [] -> []
    (c:cs) -> [(c, cs)])

instance Functor (Consumer a) where
    fmap f (Consumer q) = Consumer (\s -> [(f r, s') | (r, s') <- q s])

instance Monad (Consumer a) where
    return x = Consumer (\s -> [(x, s)])
    (Consumer p) >>= f = Consumer (\s -> concat [consume (f a) s' |
                                        (a, s') <- p s])

instance Applicative (Consumer a) where
    pure = return
    (<*>) = ap

instance Alternative (Consumer a) where
    empty = Consumer (\s -> [])
    (Consumer p) <|> (Consumer q) = Consumer (\s -> p s ++ q s)
    some p = do
        a <- p
        as <- many p
        return (a:as)
    many p = Consumer $ (\s -> case consume (some p <|> (return [])) s of
        [] -> []
        (x:xs) -> [x])


sat :: (a -> Bool) -> Consumer a a
sat p = do
    c <- item
    if p c then return c else empty


terminal :: Consumer a b -> [a] -> Maybe b
terminal foo str = case consume foo str of
    (a, _):_ -> Just a
    _ -> Nothing