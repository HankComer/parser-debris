module ConvertProgram where
import CommonData

import Data.List


getFuncName (Decl str _ _) = str

type Eval = Env -> Env -> ParseTree -> Value


getNames :: [RealDecl] -> [String]
getNames decls = fmap getFuncName (nub decls)

getFuncs :: [RealDecl] -> [[RealDecl]]
getFuncs decls =
 let
  names = getNames decls
  allNamed name = filter (\(Decl n _ _) -> name == n) decls
 in map validateNumArgs $ map allNamed names

numArgs decl = length (case decl of {Decl _ args _ -> args})

validateNumArgs :: [RealDecl] -> [RealDecl]
validateNumArgs decls =
 let
  firstArgs = numArgs (head decls)
 in case all (\a -> numArgs a == firstArgs) decls of
    True -> decls
    False -> case head decls of
      (Decl name _ _) -> error $ show name ++ " has an inconsistent number of arguments"
  

makeDeclValue :: Eval ->[RealDecl] -> Env -> Value
makeDeclValue rewrite decls globals =
 let
  transform (Decl n args body) = (TupleP args, body)
  pats = fmap transform decls
  argNum = numArgs (head decls)
  tree = unfoldArgs (map VarP (makeArgs argNum)) $ Case (Tuple (map (Atom . Id) $ makeArgs argNum)) pats
 in Thunk (\gs -> rewrite gs (Env []) tree) globals

convert :: Eval -> [RealDecl] -> Env -> (String, Value)
convert eval decls@((Decl name _ _):_) globals = (name, makeDeclValue eval decls globals)

makeGlobals :: Eval -> Env -> [RealDecl] -> Env
makeGlobals eval g decls =
 let
  things = getFuncs decls
  blah = map (\decs -> convert eval decs (squish g globals)) things
  globals = Env blah
 in globals





unfoldArgs :: [Pattern] -> ParseTree -> ParseTree
unfoldArgs [] blah = blah
unfoldArgs (a:rest) blah = Abs a (unfoldArgs rest blah)

makeArgs :: Int -> [String]
makeArgs n = makeArgs' n (replicate n "")

makeArgs' 0 a = a
makeArgs' n (a:rest) = a : makeArgs' (n - 1) (fmap ('!':) rest)