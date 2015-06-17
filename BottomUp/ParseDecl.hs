module ParseDecl where
import TokenMonad
import CommonData
import Control.Applicative


parseUnit :: Consumer Token Pattern
parseUnit = do
  sat (== LParen)
  sat (== RParen)
  return UnitP

parseTuple :: Consumer Token Pattern
parseTuple = do
  sat (== LParen)
  first <- parseWhole
  rest <- many (sat (== Comma) >> parseWhole)
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
parseWhole = parseWhole' <|> (do {sat (== LParen); blah <- parseWhole; sat (== RParen); return blah})


parseFuncDec :: Consumer Token PreDecl
parseFuncDec = do
    (Id name) <- sat isId
    args <- many parseWhole
    sat (== Equals)
    sat (== LBracket)
    contents <- many $ sat (/= RBracket)
    sat (== RBracket)
    return $ FuncDec name args contents

parseOpDec :: Consumer Token PreDecl
parseOpDec = do
    arg1 <- parseWhole
    (Op name) <- sat isOp
    arg2 <- parseWhole
    sat (== Equals)
    sat (== LBracket)
    contents <- many $ sat (/= RBracket)
    sat (== RBracket)
    return $ OpDec name arg1 arg2 contents


parseAll :: Consumer Token [PreDecl]
parseAll = many (parseFuncDec <|> ParseOpDec)



preDecl :: [Token] -> [PreDecl]
preDecl toks = case terminal parseAll toks of
    Just a -> a
    Nothing -> error "error parsing decls"