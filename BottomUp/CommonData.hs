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
  | LambdaStart
  | LambdaArrow deriving (Show, Eq)


data ParseTree = Atom Token | Apply ParseTree ParseTree | Abs String ParseTree