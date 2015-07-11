module Reformat (on, translate, getWhole, delve, reorganize) where

import Tokenize
import TokenMonad
import Control.Applicative
import CommonData


import Data.List


-- Turns all operators into function calls
-- Uses shunting yard thingy?
-- doesn't really

-- Left associative works from left to right
-- Right associative works from right to left
-- ????




getPrec :: Prec -> Int
getPrec (L _ i) = i
getPrec (R _ i) = i


getDo' :: Consumer Token [Inter -> Inter]
getDo' = do
    sat (== DoT)
    blah <- getSingle
    sat (== LCurly)
    things <- some $ do
       dingus <- processDoClause blah
       return dingus
    sat (== RCurly)
    return things

getDo :: Consumer Token Inter
getDo = do
  things <- getDo'
  let blah = (case last things Unit' of {
    Ap (Ap _ x) (Abs' [Id "_"] Unit') -> x;
    _ -> error "last statement of a do block must be an expression";})
  return (foldl (\x y -> y x) blah (init things))


    
processDoClause :: Inter -> Consumer Token (Inter -> Inter)
processDoClause op = (do {
   args <- many $ sat (\x -> x /= SemiColon && x /= Op "<-");
   sat (== Op "<-");
   blah <- getWhole;
   sat (== SemiColon);
   return (\x -> Ap (Ap op blah) (Abs' args x))} ) <|> do
    blah <- getWhole
    sat (== SemiColon)
    return (\x -> Ap (Ap op blah) (Abs' [Id "_"] x))
    


applic :: Inter -> Consumer Token Inter
applic thing = do
    blah <- getAbs <|> getTuple <|> getUnit <|> getSingle <|> getParens
    return $ Ap thing blah


getSingle = fmap Single (sat isId <|> sat isLit) <|> do
    sat (== LParen)
    (Op thing) <- sat isOp
    sat (== RParen)
    return $ Single (Id thing)

getOperator = fmap Single (sat isOp)


getApplication' thing = do
    blah <- applic thing
    getApplication' blah <|> return blah

getApplication :: Consumer Token Inter
getApplication = do
    thing <- (getSingle <|> getParens)
    getApplication' thing

getAbs :: Consumer Token Inter
getAbs = do
    sat (== LambdaStart)
    arg <- some $ sat (/= LambdaArrow)
    sat (== LambdaArrow)
    blah <- some getWhole
    return (Abs' arg (Group blah))

getTuple :: Consumer Token Inter
getTuple = do
    sat (== LParen)
    first <- getWhole
    rest <- some (sat (== Comma) >> getWhole)
    sat (== RParen)
    return (Tuple' (first : rest))

getUnit :: Consumer Token Inter
getUnit = do
    sat (== LParen)
    sat (== RParen)
    return Unit'

getCase :: Consumer Token Inter
getCase = do
    sat (== CaseStart)
    blah <- getWhole
    sat (== CaseOf)
    sat (== LCurly)
    clauses <- some getCaseClause
    sat (== RCurly)
    return (Case' blah clauses)

getCaseClause :: Consumer Token ([Token], Inter)
getCaseClause = do
    pat <- some (sat (/= LambdaArrow))
    sat (== LambdaArrow)
    blah <- getWhole
    sat (== SemiColon)
    return (pat, blah)

getLet :: Consumer Token Inter
getLet = do
    sat (== LetT)
    sat (== LCurly)
    things <- some getLetClause
    sat (== RCurly)
    sat (== InT)
    blah <- getWhole
    return (Let' things blah)

getLetClause :: Consumer Token (String, [Token], Inter)
getLetClause = do
    (Single (Id name)) <- getSingle
    pat <- many (sat (/= Equals))
    sat (== Equals)
    blah <- getWhole
    sat (== SemiColon)
    return (name, pat, blah)
    


getWhole = getApplication <|> getSingle <|> getAbs <|> getTuple <|> getUnit  <|> getOperator <|> getParens
   <|> getCase <|> getLet <|> getDo

getParens = do
    sat (== LParen)
    things <- some getWhole
    sat (== RParen)
    return (Group things)


doThing :: [Token] -> [Inter]
doThing str = case terminal (many getWhole) str of
    Just a -> a
    Nothing -> error "parse error"

f `on` blah = \a b -> f (blah a) (blah b)

-- 0 is worst
distanceFrom things (L op _) = case findIndex (== Single (Op op)) (reverse things) of
    Just a -> a
    Nothing -> 0
distanceFrom things (R op _) = case findIndex (== Single (Op op)) things of
    Just a -> a
    Nothing -> 0

highestPrec precs things = last $ sortBy (compare `on` distanceFrom things) precs' where
    precs' = head $ dropWhile (not . any (containedIn things)) $ groupBy ((==) `on` getPrec) $ sortBy (compare `on` getPrec) precs

containedIn :: [Inter] -> Prec -> Bool
containedIn things (L a _) = any (== Single (Op a)) things
containedIn things (R a _) = any (== Single (Op a)) things

findPlace :: Prec -> [Inter] -> Maybe Int
findPlace (R op _) things = findIndex (== Single (Op op)) things
findPlace (L op _) things = case findIndices (== Single (Op op)) things of
    [] -> Nothing
    blah -> Just $ last blah

reorganize :: [Prec] -> [Inter] -> Inter
reorganize precs things
 | any (\a -> case a of {Single (Op _) -> True; _ -> False}) things =
  case findPlace (highestPrec precs things) things of
   Just ind -> delve precs $ Ap (Ap theThingItself before) after where
    before = reorganize precs $ take ind things
    after = reorganize precs $ drop (ind + 1) things
    theThingItself = things !! ind
   Nothing -> Group (map (delve precs) things)
 | length things == 1 = delve precs (head things)
 | otherwise = Group things


delve precs (Group a) = reorganize precs a
delve precs (Ap a b) = Ap (delve precs a) (delve precs b)
delve precs (Single a) = Single a
delve precs (Abs' pat thing) = Abs' pat (delve precs thing)
delve precs Unit' = Unit'
delve precs (Tuple' blah) = Tuple' $ fmap (delve precs) blah
delve precs (Case' arg things) = Case' (delve precs arg) $ map (\(a, b) -> (a, delve precs b)) things
delve precs (Let' things res) = Let' (map (\(n, a, b) -> (n, a, delve precs b)) things) (delve precs res)




--doItAll :: [Prec] -> [Token] -> ParseTree
--doItAll precs toks = translate $ reorganize precs (doThing toks)






translate :: ([Token] -> [Pattern]) -> Inter -> ParseTree
translate _ (Single a) = Atom a
translate p (Ap a b) = Apply (translate p a) (translate p b)
translate p (Group [a]) = translate p a
translate _ (Group what) = error $ "Parse error? check translate " ++ show what
translate p (Abs' n blah) = Abs (case p n of {[a] -> a; a -> error "you can't have multi-arg lambdas yet"}) (translate p blah)
translate _ Unit' = Unit
translate p (Tuple' blah) = Tuple (map (translate p) blah)
translate p (Case' arg clauses) = Case (translate p arg) $ map (\(left, right) -> ((\[a] -> a) $ p left, translate p right)) clauses
translate p (Let' clauses res) = Let (map (\(a, left, right) -> (a, p left, translate p right)) clauses) (translate p res)
