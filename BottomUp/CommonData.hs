module CommonData where
import System.IO.Unsafe (unsafePerformIO)


data Token
  = Int Int
  | Double Double
  | String String
  | Id String
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Op String
  | Comma
  | Equals
  | Quote
  | LambdaStart
  | LambdaArrow deriving (Show, Eq)

isId (Id _) = True
isId _ = False

isOp (Op _) = True
isOp _ = False

isLit (Int _) = True
isLit (Double _) = True
isLit (String _) = True
isLit _ = False


data Pattern = UnitP | TupleP [Pattern] | VarP String | LitP Token | UnQuote Pattern deriving (Show, Eq)

data PreDecl = FuncDec String [Pattern] [Token] | OpDec String Pattern Pattern [Token]

data RealDecl = Decl String [Pattern] ParseTree deriving (Show, Eq)


data ParseTree = Atom Token | Apply ParseTree ParseTree | Abs Pattern ParseTree | Unit | Tuple [ParseTree] | Case ParseTree [ParseTree] deriving (Show, Eq)

data Prec = L String Int | R String Int deriving (Read, Show)

precGetOpStr :: Prec -> String
precGetOpStr (L a _) = a
precGetOpStr (R a _) = a




data Value
  = Thunk (Env -> Value) Env
  | IntV Int
  | DoubleV Double
  | StringV String
  | NameV String
  | UnitV
  | TupleV [Value]
  | Lam (Value -> Value)
  | IO (IO Value)

instance Show Value where
  show (Thunk f a) = "Thunk " ++ show (f a)
  show (IntV a) = show a
  show (DoubleV a) = show a
  show UnitV = "()"
  show (TupleV a) = show a
  show (Lam _) = "<function>"
  show (StringV a) = show a
  show (IO a) = case unsafePerformIO a of
    UnitV -> ""
    b -> "IO " ++ show b



newtype Env = Env [(String, Value)] deriving (Show)
squish :: Env -> Env -> Env
squish (Env a) (Env b) = Env $ b ++ a
