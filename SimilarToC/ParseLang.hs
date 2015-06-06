module ParseLang where
import Lang
import ParserType
import Data.Char (isAlpha, isDigit)
import Control.Applicative



  
  
   


parseProgram :: Parser Program
parseProgram = undefined

parseExpr :: Parser EComma
parseExpr = parseEComma

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
    <|> spaced parseArrayLit
    <|> spaced parseEmptyStruct
    <|> do
        spaced (char '(')
        x <- parseExpr
        spaced (char ')')
        return (Parens x)

parseArrayLit :: Parser ELeaf
parseArrayLit = do
    char '['
    x <- parseEComma
    char ']'
    return $ ArrLit x

parseEmptyStruct :: Parser ELeaf
parseEmptyStruct = do
    char '{'
    space
    char '}'
    return EmptyStruct

parsePostOps :: Parser ESuffix
parsePostOps = do
    x <- parseESuffix
    a <- spaced $ oneOf ["++", "--"]
    return $ case a of
        "++" -> ESpp x
        "--" -> ESmm x
parseIndex :: Parser ESuffix
parseIndex = do
    a <- spaced parseESuffix
    char '['
    x <- parseExpr
    spaced $ char ']'
    return (EIndex a x)
parseFuncWithArgs :: Parser ESuffix
parseFuncWithArgs = do
    func <- parseESuffix
    char '('
    args <- parseExpr
    spaced $ char ')'
    return (ESFuncArgs func args)

parseMember :: Parser ESuffix
parseMember = do
    struct <- parseESuffix
    char '.'
    mem <- spaced parseIdentifier
    return $ ESMember struct mem

parseFuncWithoutArgs :: Parser ESuffix
parseFuncWithoutArgs = do
    func <- parseESuffix
    char '('
    spaced $ char ')'
    return $ EFuncCall func

parseESuffix :: Parser ESuffix
--parseESuffix = spaced $ parseIndex <|> parsePostOps <|> parseMember <|> parseFuncWithArgs <|> parseFuncWithoutArgs <|> fmap E9 parseELeaf
parseESuffix = spaced $ fmap E9 parseELeaf <|> do
  thing <- parseESuffix
  x <- spaced $ oneOf ["++", "--", ".", "(", "["]
  case x of
   "++" -> return $ ESpp thing
   "--" -> return $ ESmm thing
   "." -> do
     mem <- spaced parseIdentifier
     return $ ESMember thing mem
   "[" -> do
     ind <- parseExpr
     char ']'
     return $ EIndex thing ind
   "(" -> (char ')' >> return (EFuncCall thing)) <|> do
     args <- parseExpr
     char ')'
     return $ ESFuncArgs thing args
    
    
    

parsePreOps :: Parser EPrefix
parsePreOps = do
    a <- spaced $ oneOf ["++", "--", "-", "!"]
    x <- parseEPrefix
    return $ case a of
      "++" -> EPpp x
      "--" -> EPmm x
      "-" -> EPneg x
      "!" -> EPnot x

parseEPrefix :: Parser EPrefix
parseEPrefix = spaced $ parsePreOps <|> fmap E8 parseESuffix

parseEProd :: Parser EProd
parseEProd = spaced $ fmap E7 parseEPrefix <|> do
    a <- parseEProd
    b <- oneOf ["*", "/", "%"]
    c <- parseEPrefix
    return $ case b of
      "*" -> ETimes a c
      "/" -> EDivide a c
      "%" -> EMod a c

parseESum :: Parser ESum
parseESum = spaced $ fmap E6 parseEProd <|> do
    a <- parseESum
    b <- oneOf ["+", "-"]
    c <- parseEProd
    return $ case b of
      "+" -> EPlus a c
      "-" -> EMinus a c

parseECmp :: Parser ECmp
parseECmp = spaced $ fmap E5 parseESum <|> do
    a <- parseESum
    b <- oneOf [">", ">=", "<", "<="]
    c <- parseESum
    return $ case b of
      ">" -> EGt a c
      ">=" -> EGe a c
      "<" -> ELt a c
      "<=" -> ELe a c

parseEEquals :: Parser EEquals
parseEEquals = spaced $ fmap E4 parseECmp <|> do
    a <- parseEEquals
    b <- oneOf ["==", "!="]
    c <- parseECmp
    return $ case b of
      "==" -> EEquals a c
      "!=" -> ENotEquals a c

parseEAnd :: Parser EAnd
parseEAnd = spaced $ fmap E3 parseEEquals <|> do
    a <- parseEAnd
    string "&&"
    c <- parseEEquals
    return $ EAnd a c

parseEOr :: Parser EOr
parseEOr = spaced $ fmap E2 parseEAnd <|> do
    a <- parseEOr
    string "||"
    c <- parseEAnd
    return $ EOr a c

parseECond :: Parser ECond
parseECond = spaced $ fmap E1 parseEOr <|> do
    a <- parseEOr
    char '?'
    b <- parseECond
    char ':'
    c <- parseECond
    return $ ECond a b c

parseEAssign :: Parser EAssign
parseEAssign = spaced $ fmap E0 parseECond <|> do
    a <- parseESuffix
    x <- oneOf ["=", "+=", "-=", "*=", "/=", "%="]
    c <- parseEAssign
    return . EAssign $ case x of
      "=" -> SimpleEquals a c
      "+=" -> PlusEquals a c
      "-=" -> MinusEquals a c
      "*=" -> TimesEquals a c
      "/=" -> DivideEquals a c
      "%=" -> ModEquals a c
parseEComma :: Parser EComma
parseEComma = spaced $ fmap E parseEAssign <|> do
    a <- parseEComma
    char ','
    c <- parseEAssign
    return $ EComma a c