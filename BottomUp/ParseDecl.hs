module ParseDecl where
import TokenMonad
import CommonData
import Control.Applicative
import Reformat


parseUnit :: Consumer Token Pattern
parseUnit = do
  sat (== LParen)
  sat (== RParen)
  return UnitP

parseTuple :: Consumer Token Pattern
parseTuple = do
  sat (== LParen)
  first <- parseWhole
  rest <- some (sat (== Comma) >> parseWhole)
  sat (== RParen)
  return (TupleP (first:rest))

parseVar :: Consumer Token Pattern
parseVar = do
  (Id thing) <- sat isId
  return (VarP thing)

parseLit :: Consumer Token Pattern
parseLit = fmap LitP (sat isLit)

parseWhole' :: Consumer Token Pattern
parseWhole' = parseUnit <|> parseTuple <|> parseVar <|> parseLit

parseWhole :: Consumer Token Pattern
parseWhole = parseWhole' <|> (do {sat (== LParen); blah <- parseWhole; sat (== RParen); return blah}) <|> (sat (== Quote) >> fmap UnQuote parseWhole)


parsePat :: [Token] -> [Pattern]
parsePat toks = case terminal (many parseWhole) toks of
    Just a -> a
    Nothing -> error $ "Couldn't parse pattern: " ++ show toks


parseFuncDec :: Consumer Token PreDecl
parseFuncDec = do
    (Id name) <- sat isId
    args <- many parseWhole
    sat (== Equals)
    contents <- many getWhole
    sat (== SemiColon)
    return $ FuncDec name args (Group contents)

parseOpDec :: Consumer Token PreDecl
parseOpDec = do
    arg1 <- parseWhole
    (Op name) <- sat isOp
    arg2 <- parseWhole
    sat (== Equals)
    contents <- many getWhole
    sat (== SemiColon)
    return $ OpDec name arg1 arg2 (Group contents)


parseAll :: Consumer Token [PreDecl]
parseAll = many (parseFuncDec <|> parseOpDec)



preDecl :: [Token] -> [PreDecl]
preDecl toks = case terminal parseAll toks of
    Just a -> a
    Nothing -> error "error parsing decls"