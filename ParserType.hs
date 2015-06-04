module ParserType where
import Control.Applicative
import Control.Monad (ap)
import Data.Char (isDigit, isAlpha)


newtype Parser a = Parser {parse :: (String -> [(a, String)])}

item :: Parser Char
item = Parser (\s -> case s of
    "" -> []
    (c:cs) -> [(c, cs)])




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


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\s -> case parse (p <|> q) s of
    [] -> []
    (x:xs) -> [x])

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

oneOf :: String -> Parser Char
oneOf [] = empty
oneOf (c:cs) = char c <|> oneOf cs

