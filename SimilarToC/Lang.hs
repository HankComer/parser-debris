module Lang where
import Control.Applicative
import ParserType



data Program = Program [Stmt] deriving (Show, Eq)

data Stmt
  = FuncDecl Identifier [Identifier] [Stmt]
  | Assign Assign
  | Declare Identifier
  | Perform EComma deriving (Show, Eq)

data Assign
  = SimpleEquals ESuffix EAssign
  | PlusEquals ESuffix EAssign
  | MinusEquals ESuffix EAssign
  | TimesEquals ESuffix EAssign
  | DivideEquals ESuffix EAssign
  | ModEquals ESuffix EAssign deriving (Show, Eq)


data EComma = E EAssign | EComma EComma EAssign deriving (Show, Eq)
data EAssign = E0 ECond | EAssign Assign deriving (Show, Eq)
data ECond = E1 EOr | ECond EOr ECond ECond deriving (Show, Eq)
data EOr = E2 EAnd | EOr EOr EAnd deriving (Show, Eq)
data EAnd = E3 EEquals | EAnd EAnd EEquals deriving (Show, Eq)
data EEquals = E4 ECmp | EEquals EEquals ECmp | ENotEquals EEquals ECmp deriving (Show, Eq)
data ECmp = E5 ESum | EGt ESum ESum | EGe ESum ESum | ELt ESum ESum | ELe ESum ESum deriving (Show, Eq)
data ESum = E6 EProd | EPlus ESum EProd | EMinus ESum EProd deriving (Show, Eq)
data EProd = E7 EPrefix | ETimes EProd EPrefix | EDivide EProd EPrefix | EMod EProd EPrefix deriving (Show, Eq)
data EPrefix = E8 ESuffix | EPpp EPrefix | EPmm EPrefix | EPneg EPrefix | EPnot EPrefix deriving (Show, Eq)
data ESuffix = E9 ELeaf | ESpp ESuffix | ESmm ESuffix | ESFuncArgs ESuffix EComma
  | ESMember ESuffix Identifier | EFuncCall ESuffix | EIndex ESuffix EComma deriving (Show, Eq)
data ELeaf = Id Identifier | Num Number | String String | Parens EComma deriving (Show, Eq)

type Identifier = String

data Number = I Int | D Double deriving (Show, Eq)
