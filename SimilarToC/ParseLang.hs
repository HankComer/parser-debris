module ParseLang where
import Lang
import ParserType
import Data.Char (isAlpha, isDigit)
import Control.Applicative


getToken :: Parser String
getToken = Parser lex

getValue :: Read a => Parser a
getValue = Parser reads


spaced thing = Parser $ (\s -> let
  blah = parse (fmap (parse thing) getToken) s
  foo = (fmap (\(a, b) -> (fmap fst a, b))) $ fmap (\(a, b) -> (filter ((== "") . snd) a, b)) blah
  bar = concat $ fmap (\(as, b) -> zip as (repeat b)) foo
 in bar)
  
  
   


parseProgram :: Parser Program
parseProgram = undefined

parseExpr :: Parser EComma
parseExpr = undefined

parseInt :: Parser Int
parseInt = Parser reads

parseDouble :: Parser Double
parseDouble = Parser reads

parseIdentifier :: Parser Identifier
parseIdentifier = do
     h <- (sat isAlpha) <|> char '_'
     rest <- many (sat isAlpha <|> sat isDigit <|> char '_')
     return (h:rest)

parseELeaf :: Parser ELeaf
parseELeaf = (fmap Id (spaced parseIdentifier))
    <|> (fmap (Num . I) (spaced parseInt)) 
    <|> (fmap (Num . D) (spaced parseDouble))
    <|> (fmap String (spaced parseString))
    <|> do
        spaced (char '(')
        x <- parseExpr
        spaced (char ')')
        return (Parens x)

parsePostOps :: Parser ESuffix
parsePostOps = do
    x <- parseESuffix
    a <- oneOf ["++", "--"]
    return $ case a of
        "++" -> ESpp x
        "--" -> ESmm x
parseIndex :: Parser ESuffix
parseIndex = do
    a <- spaced parseESuffix
    spaced $ char '['
    x <- parseExpr
    spaced $ char ']'
    return (EIndex a x)

parseFuncArgs :: Parser [EAssign]
parseFuncArgs = empty

parseESuffix :: Parser ESuffix
parseESuffix = parseIndex <|> parsePostOps 