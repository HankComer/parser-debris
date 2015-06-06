module TokenLang where



data Token a
 = Op String
 | LParen
 | RParen
 | LBracket
 | RBracket
 | LBrace
 | RBrace
 | Z Int
 | R Double
 | S String
 | N String
 | EOF
 | AST a deriving (Show, Eq)



