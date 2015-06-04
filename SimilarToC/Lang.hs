module Lang where
import Control.Applicative
import ParserType



data Program = Program [Stmt] deriving (Show, Eq)

data Stmt
  = FuncDecl Identifier [Identifier] [Stmt]
  | Assign Assign
  | Perform Expr deriving (Show, Eq)

data Assign
  = SimpleEquals Identifier Expr
  | PlusEquals Identifier Expr
  | MinusEquals Identifier Expr
  | TimesEquals Identifier Expr
  | DivideEquals Identifier Expr
  | ModEquals Identifier Expr deriving (Show, Eq)

data Expr = E0 ECond | EComma Expr ECond deriving (Show, Eq)
data ECond = E1 EOr | ECond EOr ECond ECond deriving (Show, Eq)
data EOr = E2 EAnd | EOr EOr EAnd deriving (Show, Eq)
data EAnd = E3 EEquals | EAnd EAnd EEquals deriving (Show, Eq)
data EEquals = E4 ECmp | EEquals EEquals ECmp | ENotEquals EEquals ECmp deriving (Show, Eq)
data ECmp = E5 ESum | EGt ESum ESum | EGe ESum ESum | ELt ESum ESum | ELe ESum ESum deriving (Show, Eq)
data ESum = E6 EProd | EPlus ESum EProd | EMinus ESum EProd deriving (Show, Eq)
data EProd = E7 EPrefix | ETimes EProd EPrefix | EDivide EProd EPrefix | EMod EProd EPrefix deriving (Show, Eq)
data EPrefix = E8 ESuffix | EPpp EPrefix | EPmm EPrefix | EPneg EPrefix | EPnot EPrefix deriving (Show, Eq)
data ESuffix = E9 ELeaf | ESpp ESuffix | ESmm ESuffix | ESFuncArgs ESuffix Expr
  | ESMember ESuffix Identifier | EFuncCall ESuffix | EIndex ESuffix Expr deriving (Show, Eq)
data ELeaf = Id Identifier | Num Number | Str String | Parens Expr deriving (Show, Eq)

type Identifier = String

data Number = Int Int | Double Double deriving (Show, Eq)
