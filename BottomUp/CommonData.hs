module CommonData where


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


data Pattern = UnitP | TupleP [Pattern] | VarP String | LitP Token deriving (Show, Eq)

data PreDecl = FuncDec String [Pattern] [Token] | OpDec String Pattern Pattern [Token]

data RealDecl = Decl String [Pattern] ParseTree deriving (Show, Eq)


data ParseTree = Atom Token | Apply ParseTree ParseTree | Abs String ParseTree deriving (Show, Eq)

data Prec = L String Int | R String Int deriving (Read, Show)

precGetOpStr :: Prec -> String
precGetOpStr (L a _) = a
precGetOpStr (R a _) = a