module Tokenize where
import ParserType
import Data.Char (isAlpha, isDigit)
import Control.Applicative

data Token
  = Int Int
  | Double Double
  | String String
  | Id String
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Op String
  | Comma deriving (Show, Eq)



getInt :: Parser Token
getInt = fmap Int (Parser reads)

getDouble :: Parser Token
getDouble = fmap Double (Parser reads)

getString :: Parser Token
getString = fmap String (Parser reads)

getId :: Parser Token
getId = spaced $ do
    first <- some (sat isAlpha <|> char '_')
    rest <- many (sat isAlpha <|> sat isDigit <|> char '_')
    return (Id $ first ++ rest)

getOp :: Parser Token
getOp = spaced $ do
    things <- some (oneOf $ fmap return "+=-~!#$%^&*<>.?/:|")
    return (Op (concat things))

getSymbol :: Parser Token
getSymbol = spaced $ do
    thing <- item
    case thing of
        '(' -> return LParen
        ')' -> return RParen
        '[' -> return LBracket
        ']' -> return RBracket
        ',' -> return Comma
        _ -> empty



getTokens :: Parser [Token]
getTokens = many $ getInt <|> getDouble <|> getString <|> getId <|> getOp <|> getSymbol

tokenize :: String -> [Token]
tokenize str = case terminal getTokens str of
    Just a -> a
    Nothing -> error "syntax error"