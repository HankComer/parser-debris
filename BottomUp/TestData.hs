{-# LANGUAGE StandaloneDeriving #-}
module TestData where
import Test.QuickCheck
import CommonData
import Control.Applicative


arbitraryIdentifier :: Gen String
arbitraryIdentifier = fmap (take 7) $ infiniteListOf $ choose ('a', 'z')

randomListOf a = do
  x <- choose (0, 3)
  fmap (take x) (infiniteListOf a)


arbitraryNumLiteral = oneof [fmap Double arbitrarySizedFractional, fmap Int arbitrarySizedBoundedIntegral]

arbitraryStringToken = fmap (String . show) $ fmap (take 5) $ infiniteListOf $ choose ('\10', '~')






instance Arbitrary Pattern where
 arbitrary = do
  blah <- choose (0, 3 :: Int)
  case blah of
   0 -> return UnitP
   1 -> do
    n <- choose (2, 4)
    fmap (TupleP . take n) infiniteList
   2 -> fmap VarP arbitraryIdentifier
   3 -> fmap LitP $ oneof [arbitraryNumLiteral, arbitraryStringToken, arbitraryNumLiteral]


instance Arbitrary ParseTree where
 arbitrary = do
  blah <- choose (0, 8 :: Int)
  case blah of
   0 -> fmap Atom $ oneof [arbitraryNumLiteral, arbitraryNumLiteral, arbitraryStringToken, fmap Id arbitraryIdentifier]
   1 -> do
    a <- arbitrary
    b <- arbitrary
    return (Apply a b)
   2 -> do
    a <- arbitrary
    b <- arbitrary
    return (Abs a b)
   3 -> return Unit
   4 -> do
    n <- choose (2, 4 :: Int)
    fmap (Tuple . take n) infiniteList
   5 -> generateCases
   6 -> generateLets
   _ -> return Unit
arbitraryArgPatterns :: Gen [Pattern]
arbitraryArgPatterns = do
    n <- choose (0, 3 :: Int)
    fmap (take n) infiniteList


generateCases = do
    arg <- arbitrary
    n <- choose (1, 4 :: Int)
    (Case arg . take n) <$> infiniteListOf ((,) <$> arbitrary <*> arbitrary)

generateLets = do
    res <- arbitrary
    n <- choose (1, 4 :: Int)
    (flip Let res . take n) <$> infiniteListOf ((,,) <$> arbitraryIdentifier <*> arbitraryArgPatterns <*> arbitrary)