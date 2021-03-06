module CommonData where
import System.IO.Unsafe (unsafePerformIO)
import System.IO



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
  | SemiColon
  | LambdaStart
  | CaseStart
  | CaseOf
  | LCurly
  | RCurly
  | LetT
  | InT
  | LambdaArrow
  | Import String
  | DoT
  | VarBindArrow
  | Prec Prec deriving (Show, Eq)

isPrec (Prec _) = True
isPrec _ = False

isImport (Import _) = True
isImport _ = False

isId (Id _) = True
isId _ = False

isOp (Op _) = True
isOp _ = False

isLit (Int _) = True
isLit (Double _) = True
isLit (String _) = True
isLit _ = False


data Pattern = UnitP | TupleP [Pattern] | VarP String | LitP Token | UnQuote Pattern deriving (Show, Eq)

data Inter = Ap Inter Inter | Single Token | Group [Inter] | Abs' [Token] Inter
  | Tuple' [Inter] | Unit' | Case' Inter [([Token], Inter)] | Let' [(String, [Token], Inter)] Inter deriving (Show, Eq)

data PreDecl = FuncDec String [Pattern] Inter | OpDec String Pattern Pattern Inter deriving (Show)

data RealDecl = Decl String [Pattern] ParseTree deriving (Show, Eq)


data ParseTree = Atom Token | Apply ParseTree ParseTree | Abs Pattern ParseTree | Unit | Tuple [ParseTree]
  | Case ParseTree [(Pattern, ParseTree)] | Let [(String, [Pattern], ParseTree)] ParseTree deriving (Show, Eq)

data Prec = L String Int | R String Int deriving (Read, Show, Eq)

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
  | Error Value
  | Handle Handle

unHandle (Handle a) = a
unHandle (Thunk f a) = unHandle (f a)
unHandle (Error a) = stop a
unHandle a = error $ "type error: expected Int, found " ++ show a

unInt (IntV i) = i
unInt (Thunk f a) = unInt (f a)
unInt (Error a) = stop a
unInt a = error $ "type error: expected Int, found " ++ show a

unDouble (DoubleV i) = i
unDouble (Thunk f a) = unDouble (f a)
unDouble (IntV i) = fromIntegral i
unDouble (Error a) = stop a
unDouble a = error $ "type error: expected Double, found " ++ show a

unString (StringV i) = i
unString (Thunk f a) = unString (f a)
unString (Error a) = stop a
unString a = error $ "type error: expected String, found " ++ show a

unLam (Lam f) = f
unLam (Thunk f a) = unLam (f a)
unLam (Error a) = stop a
unLam a = error $ "type error: expected function, found " ++ show a

unIO (IO a) = a
unIO (Thunk f a) = unIO (f a)
unIO (Error a) = stop a
unIO a = error $ "type error: expected IO value, found " ++ show a

unError (Error a) catcher = unLam catcher a
unError (Thunk f a) catcher = unError (f a) catcher
unError ok _ = ok

stop :: Value -> a
stop (Thunk f a) = stop (f a)
stop (StringV a) = error a
stop (IO a) = unsafePerformIO (a >> error "\nUncaught exception")
stop a = error (show a)

instance Show Value where
  show (Error a) = stop a
  show (Thunk f a) = {-"Thunk " ++ -}show (f a)
  show (IntV a) = show a
  show (DoubleV a) = show a
  show UnitV = "()"
  show (TupleV a) = show a
  show (Lam _) = "<function>"
  show (StringV a) = show a
  show (IO a) = case unsafePerformIO a of
    UnitV -> ""
    b -> "IO " ++ show b

instance Eq Value where
  (Error a) == _ = stop a
  _ == (Error b) = stop b
  (Thunk f a) == b = (f a) == b
  a == (Thunk f b) = a == (f b)
  (IntV a) == (IntV b) = a == b
  (DoubleV a) == (DoubleV b) = a == b
  (StringV a) == (StringV b) = a == b
  (TupleV a) == (TupleV b) = a == b
  UnitV == UnitV = True
  _ == _ = False



newtype Env = Env [(String, Value)] deriving (Show)
squish :: Env -> Env -> Env
squish (Env a) (Env b) = Env $ b ++ a
