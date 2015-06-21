module ParserType where
import Control.Applicative
import Control.Monad (ap)
import Data.Char (isDigit, isAlpha)


newtype Parser a = Parser {parse :: (String -> [(a, String)])}

item :: Parser Char
item = Parser (\s -> case s of
    "" -> []
    (c:cs) -> [(c, cs)])
getToken :: Parser String
getToken = Parser lex

getValue :: Read a => Parser a
getValue = Parser reads

spaced :: Parser a -> Parser a
spaced thing = Parser $ (\s -> let
  blah = parse (fmap (parse thing) getToken) s
  foo = (fmap (\(a, b) -> (fmap fst a, b))) $ fmap (\(a, b) -> (filter ((== "") . snd) a, b)) blah
  bar = concat $ fmap (\(as, b) -> zip as (repeat b)) foo
 in bar)

line :: Parser String
line = do
    blah <- many (sat (/= '\n'))
    char '\n'
    return blah



instance Functor Parser where
    fmap f (Parser q) = Parser (\s -> [(f r, s') | (r, s') <- q s])

instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    (Parser p) >>= f = Parser (\s -> concat [parse (f a) s' |
                                        (a, s') <- p s])

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Alternative Parser where
    empty = Parser (\s -> [])
    (Parser p) <|> (Parser q) = Parser (\s -> p s ++ q s)
    some p = do
        a <- p
        as <- many p
        return (a:as)
    many p = do
        some p +++ return []

terminal :: Parser a -> String -> Maybe a
terminal foo str = case parse foo str of
    (a, _):_ -> Just a
    _ -> Nothing


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\s -> case parse (p <|> q) s of
    [] -> []
    (x:xs) -> [x])

infixl 3 <!>
(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = Parser (\s -> case parse p s of
    [] -> parse q s
    a -> a)

space :: Parser ()
space = do
    many (char ' ')
    return ()

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

matchStringChar = (do {char '\\'; a <- item; return (read ['\'', '\\', a, '\''] :: Char)}) <|> (sat (/= '"'))

parseString :: Parser String
parseString = do
    char '"'
    things <- many matchStringChar
    char '"'
    return things



oneOf :: [String] -> Parser String
oneOf [] = empty
oneOf (c:cs) = string c <|> oneOf cs

