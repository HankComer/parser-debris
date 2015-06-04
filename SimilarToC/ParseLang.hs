module ParseLang where
import Lang
import ParserType
import Data.Char (isAlpha, isDigit)
import Control.Applicative


getToken :: Parser String
getToken = Parser lex


parseProgram :: Parser Program
parseProgram = undefined

parseInt :: Parser Int
parseInt = Parser reads

parseDouble :: Parser Double
parseDouble = Parser reads

parseIdentifier :: Parser Identifier
parseIdentifier = do
     h <- (sat isAlpha) <|> char '_'
     rest <- many (sat isAlpha <|> sat isDigit <|> char '_')
     return (h:rest)

