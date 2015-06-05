module Simplicity where

import Lang



data Expr
  = Single Atom
  | Prefix String Expr
  | Postfix String Expr
  | Index Expr Expr
  | Member Expr Identifier
  | Call Expr
  | CallArgs Expr [Expr]
  | Infix String Expr Expr
  | Trinary Expr Expr Expr deriving (Show, Eq)

data Atom = Int Int | Doub Double | Str String | Ident Identifier deriving (Show, Eq)

class Translate a where
    translate :: a -> Expr

instance Translate ELeaf where
  translate (Id a) = Single (Ident a)
  translate (Num (I a)) = Single (Int a)
  translate (Num (D a)) = Single (Doub a)
  translate (String a) = Single (Str a)
  translate (Parens a) = translate a

instance Translate ESuffix where
  translate (E9 a) = translate a
  translate (ESpp a) = Postfix "++" (translate a)
  translate (ESmm a) = Postfix "--" (translate a)
  translate (ESFuncArgs a b) = CallArgs (translate a) (arglist b)
  translate (ESMember a b) = Member (translate a) b
  translate (EFuncCall a) = Call (translate a)
  translate (EIndex a b) = Index (translate a) (translate b)

arglist :: EComma -> [Expr]
arglist a = reverse (map translate $ arglist' a) where
   arglist' :: EComma -> [EAssign]
   arglist' (E a) = [a]
   arglist' (EComma a b) = b : arglist' a

instance Translate EPrefix where
  translate (E8 a) = translate a
  translate (EPpp a) = Prefix "++" (translate a)
  translate (EPmm a) = Prefix "--" (translate a)
  translate (EPneg a) = Prefix "-" (translate a)
  translate (EPnot a) = Prefix "!" (translate a)

instance Translate EProd where
  translate (E7 a) = translate a
  translate (ETimes a b) = Infix "*" (translate a) (translate b)
  translate (EDivide a b) = Infix "/" (translate a) (translate b)
  translate (EMod a b) = Infix "%" (translate a) (translate b)

instance Translate ESum where
  translate (E6 a) = translate a
  translate (EPlus a b) = Infix "+" (translate a) (translate b)
  translate (EMinus a b) = Infix "-" (translate a) (translate b)

instance Translate ECmp where
  translate (E5 a) = translate a
  translate (EGt a b) = Infix ">" (translate a) (translate b)
  translate (EGe a b) = Infix ">=" (translate a) (translate b)
  translate (ELt a b) = Infix "<" (translate a) (translate b)
  translate (ELe a b) = Infix "<=" (translate a) (translate b)

instance Translate EEquals where
  translate (E4 a) = translate a
  translate (EEquals a b) = Infix "==" (translate a) (translate b)
  translate (ENotEquals a b) = Infix "!" (translate a) (translate b)

instance Translate EAnd where
  translate (E3 a) = translate a
  translate (EAnd a b) = Infix "&&" (translate a) (translate b)

instance Translate EOr where
  translate (E2 a) = translate a
  translate (EOr a b) = Infix "||" (translate a) (translate b)

instance Translate ECond where
  translate (E1 a) = translate a
  translate (ECond a b c) = Trinary (translate a) (translate b) (translate c)

instance Translate EAssign where
  translate (E0 a) = translate a
  translate (EAssign a) = case a of
    SimpleEquals a b -> Infix "=" (translate a) (translate b)
    PlusEquals a b -> Infix "+=" (translate a) (translate b)
    MinusEquals a b -> Infix "-=" (translate a) (translate b)
    TimesEquals a b -> Infix "*=" (translate a) (translate b)
    DivideEquals a b -> Infix "/=" (translate a) (translate b)
    ModEquals a b -> Infix "%=" (translate a) (translate b)

instance Translate EComma where
  translate (E a) = translate a
  translate (EComma a b) = Infix "," (translate a) (translate b)

