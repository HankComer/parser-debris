module Tokenize where
import ParserType
import Data.Char (isAlpha, isDigit)
import Control.Applicative
import CommonData




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
    things <- some (oneOf $ fmap return "+-~#$%^&*<>.?/:|")
    things' <- many (oneOf $ fmap return "+-=~#$%^&*<>.?/:|")
    return (Op (concat things ++ concat things'))

getSymbol :: Parser Token
getSymbol = spaced $ do
    thing <- item
    case thing of
        '(' -> return LParen
        ')' -> return RParen
        '[' -> return LBracket
        ']' -> return RBracket
        '!' -> return LambdaStart
        '-' -> char '>' >> return LambdaArrow
        ',' -> return Comma
        '=' -> return Equals
        _ -> empty





getTokens :: Parser [Token]
getTokens = many $ getInt <|> getDouble <|> getString <|> getId <|> getOp <|> getSymbol

removeComment :: String -> String
removeComment ('/':'/':_) = []
removeComment (a:rest) = a:removeComment rest

getPrecs :: [String] -> [Prec]
getPrecs = fmap read

codeAndMeta :: String -> (String, [String])
codeAndMeta stuff =
 let
  blah = fmap removeComment $ lines stuff
  meta = fmap (tail . dropWhile (/= '@')) blah
  code = fmap (takeWhile (/= '@')) blah
 in (unlines code, meta)
  
  

tokenize :: String -> [Token]
tokenize str = case terminal getTokens str of
    Just a -> a
    Nothing -> error "syntax error"

tokensAndPrecs :: String -> ([Token], [Prec])
tokensAndPrecs str = case codeAndMeta str of
    (code, meta) -> (tokenize code, getPrecs meta)


backToString' :: Token -> String
backToString' (Int i) = ' ':show i
backToString' (String a) = show a
backToString' (Double i) = ' ' : show i
backToString' (Id a) = a ++ " "
backToString' (Op a) = ' ':a ++ " "
backToString' LParen = "("
backToString' RParen = ")"
backToString' LBracket = "["
backToString' RBracket = "]"
backToString' LambdaStart = "!"
backToSTring' LambdaArrow = " -> "

backToString :: [Token] -> String
backToString = (>>= backToString')