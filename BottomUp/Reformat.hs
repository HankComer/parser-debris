module Reformat (rearrange, on) where

import Tokenize
import TokenMonad
import Control.Applicative

import Data.List


-- Turns all operators into function calls
-- Uses shunting yard thingy?

-- Left associative works from left to right
-- Right associative works from right to left



data Prec = L String Int | R String Int




data Inter = Ap Inter Inter | Single Token | Group [Inter] deriving (Show, Eq)



applic :: Inter -> Consumer Token Inter
applic thing = do
    blah <- getSingle <|> getParens
    return $ Ap thing blah


getSingle = fmap Single (sat isId <|> sat isLit)

getOperator = fmap Single (sat isOp)


getApplication' thing = do
    blah <- applic thing
    getApplication' blah <|> return blah

getApplication :: Consumer Token Inter
getApplication = do
    thing <- getSingle
    getApplication' thing


getWhole = getApplication <|> getSingle <|> getOperator <|> getParens

getParens = do
    sat (== LParen)
    things <- many getWhole
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

highestPrec precs things = last $ sortBy (compare `on` distanceFrom things) precs

findPlace :: Prec -> [Inter] -> Int
findPlace (L op _) things = case findIndex (== Single (Op op)) things of
    Just a -> a
findPlace (R op _) things = last $ findIndices (== Single (Op op)) things

reorganize precs things
 | any (\a -> case a of {Single (Op _) -> True; _ -> False}) things =
  let
   ind = findPlace (highestPrec precs things) things
   before = reorganize precs $ take ind things
   after = reorganize precs $ drop (ind + 1) things
   theThingItself = things !! ind
  in Ap (Ap theThingItself before) after
 | length things == 1 = delve precs (head things)


delve precs (Group a) = Group [reorganize precs a]
delve precs (Ap a b) = Ap (delve precs a) (delve precs b)
delve precs (Single a) = Single a




reify' :: Inter -> [Token]
reify' (Group a) = LParen : (reify a) ++ [RParen]
reify' (Single a) = [a]
reify' (Ap a b) = LParen : reify' a ++ reify' b ++ [RParen]

reify = (>>= reify')

format :: String -> [Inter]
format = doThing . tokenize



rearrange :: [Prec] -> String -> [Token]
rearrange precs = reify . (return . reorganize precs) . format