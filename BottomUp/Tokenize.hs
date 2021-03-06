module Tokenize where
import ParserType
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative
import CommonData
import Data.List (isPrefixOf)




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
    return $ case first ++ rest of
        "case" -> CaseStart
        "of" -> CaseOf
        "in" -> InT
        "let" -> LetT
        "do" -> DoT
        a -> Id a

getOp :: Parser Token
getOp = spaced $ do
    things <- some (oneOf $ fmap return "+-~=#$%^&*<>.?/:|")
    things' <- many (oneOf $ fmap return "+-=~#$%^&*<>.?/:|")
    return (Op (concat things ++ concat things'))

scanPrec :: Parser Token
scanPrec = do
    many (sat isSpace)
    char '@'
    blah <- line
    case reads blah of
        (a, _):_ -> return (Prec a)
        _ -> empty

scanImport = do
  many (sat isSpace)
  string "import"
  space
  char '<'
  blah <- many (sat (/= '>'))
  char '>'
  many (sat isSpace)
  return (Import $ '<':blah ++ ">")
  

getSymbol :: Parser Token
getSymbol = do
    space
    thing <- item
    many (sat isSpace)
    case thing of
        '(' -> return LParen
        ')' -> return RParen
        '[' -> return LBracket
        ']' -> return RBracket
        '!' -> return LambdaStart
        '-' -> char '>' >> return LambdaArrow
        ',' -> return Comma
        '=' -> return Equals
        '\'' -> return Quote
        ';' -> return SemiColon
        '{' -> return LCurly
        '}' -> return RCurly
        '\\' -> error "You've got a backslash, and probably mean '!'."
        _ -> empty





getTokens :: Parser [Token]
getTokens = many $ getInt <|> getDouble <|> scanImport <|> getString <|> getId <|> getOp <|> getSymbol <|> scanPrec 

removeComment :: String -> String
removeComment [] = []
removeComment ('/':'/':_) = []
removeComment (a:rest) = a:removeComment rest

removeComments = unlines . map removeComment . lines

{-
removeImports :: String -> ([String], String)
removeImports str = 
 let
  things = map (dropWhile isSpace) (lines str)
  imports = fmap (drop 6) $ filter (isPrefixOf "import") things
  rest = unlines $ filter (not . isPrefixOf "import") things
 in (imports, rest)

getPrecs :: [String] -> [Prec]
getPrecs strs = fmap read (filter (/= "") strs)


codeAndMeta :: String -> (String, [String])
codeAndMeta stuff =
 let
  blah = fmap removeComment $ lines stuff
  meta = fmap (\l -> case dropWhile (/= '@') l of {'@':rest -> rest; a -> a}) blah
  code = fmap (takeWhile (/= '@')) blah
 in (unlines code, meta)
-}
  
-- Assumes that missing prec means left associative, precedence 9
generateMissingPrecs :: [Prec] -> [Token] -> [Prec]
generateMissingPrecs precs toks =
 let
  things = filter isOp toks
  notPresent (Op a) = not $ a `elem` fmap precGetOpStr precs
  notThere = filter notPresent things
 in precs ++ fmap (\(Op foo) -> L foo 9) notThere


tokenize :: String -> [Token]
tokenize str = case terminal getTokens (removeComments str) of
    Just a -> map (\foo -> case foo of {Op "->" -> LambdaArrow; Op "=" -> Equals; Op "<-" -> VarBindArrow; blah -> blah}) a
    Nothing -> error "syntax error"



tokensPrecsImports :: String -> ([Token], [Prec], [String])
tokensPrecsImports str =
 let
  toks = tokenize str
  imports = fmap (\(Import x) -> x) $ filter isImport toks
  precs = generateMissingPrecs (fmap (\(Prec x) -> x) $ filter isPrec toks) tokens
  tokens = filter (\x -> not $ isPrec x || isImport x) toks
 in (tokens, precs, imports)


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