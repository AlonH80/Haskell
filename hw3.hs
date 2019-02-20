-- Alon Hartanu
-- ID: 305143422

import Data.List
import Data.Char

-- Question 1 --
type Variable = String
type Value = Float
data Expr = Const Value -- a constant number
        | Add Expr Expr -- addition of expressions
        | Mul Expr Expr -- multiplication of expressions
        | Sub Expr Expr -- subtraction of expressions
        | Div Expr Expr -- division of exp
        | Var Variable -- a variable
type Dictionary = [(Variable, Value)]
type EvalError = [Variable]
type EvalResult = Either EvalError Value

-- Section a --
display::Expr->String
display (Const x) = show x
display (Var x)=x
display (Add x y)='(':(display x)++('+':(display y))++")"
display (Mul x y)='(':(display x)++('*':(display y))++")"
display (Sub x y)='(':(display x)++('-':(display y))++")"
display (Div x y)='(':(display x)++('/':(display y))++")"

-- Section b --
addResult::EvalResult->EvalResult->EvalResult
addResult (Left x) (Right y)=Left x
addResult (Right x) (Left y)=Left y
addResult (Left x) (Left y)=Left (x++y)
addResult (Right x) (Right y)=Right (x+y)

subResult::EvalResult->EvalResult->EvalResult
subResult (Left x) (Right y)=Left x
subResult (Right x) (Left y)=Left y
subResult (Left x) (Left y)=Left (x++y)
subResult (Right x) (Right y)=Right (x-y)

mulResult::EvalResult->EvalResult->EvalResult
mulResult (Left x) (Right y)=Left x
mulResult (Right x) (Left y)=Left y
mulResult (Left x) (Left y)=Left (x++y)
mulResult (Right x) (Right y)=Right (x*y)

divResult::EvalResult->EvalResult->EvalResult
divResult (Left x) (Right y)=Left x
divResult (Right x) (Left y)=Left y
divResult (Left x) (Left y)=Left (x++y)
divResult (Right x) (Right y)=Right (x/y)

getVal::Expr->Dictionary->Expr
getVal (Var x) []=(Var x)
getVal (Var x) ((var,val):keys) = if x==var
                                  then (Const val)
                                  else getVal (Var x) keys

calculateExpr::Expr->EvalResult
calculateExpr (Var x)=Left [x]
calculateExpr (Const a)=Right a
calculateExpr (Add (Const a) (Const b))= Right (a+b)
calculateExpr (Mul (Const a) (Const b))= Right (a*b)
calculateExpr (Sub (Const a) (Const b))= Right (a-b)
calculateExpr (Div (Const a) (Const b))= Right (a/b)

eval::Dictionary->Expr->EvalResult
eval _ (Const a) = Right a
eval vars (Var a) = calculateExpr (getVal (Var a) vars)
eval _ (Add (Const a) (Const b)) =calculateExpr (Add (Const a) (Const b))
eval vars (Add x y) = addResult (eval vars x) (eval vars y)
eval _ (Sub (Const a) (Const b)) =calculateExpr (Sub (Const a) (Const b))
eval vars (Sub x y) = subResult (eval vars x) (eval vars y)
eval _ (Mul (Const a) (Const b)) =calculateExpr (Mul (Const a) (Const b))
eval vars (Mul x y) = mulResult (eval vars x) (eval vars y)
eval _ (Div (Const a) (Const b)) =calculateExpr (Div (Const a) (Const b))
eval vars (Div x y) = divResult (eval vars x) (eval vars y)


-- Question 2 --
data Tree a b = Leaf b | Node a (Tree a b) (Tree a b) deriving (Show)

-- Section a --
reverseTree::Tree a b->Tree a b
reverseTree (Leaf x)=(Leaf x)
reverseTree (Node n (Leaf x) (Leaf y))=(Node n (Leaf y) (Leaf x))
reverseTree (Node n x y)=(Node n (reverseTree y) (reverseTree x))

-- Section b --
areTreesEqual::Tree Int Char->Tree Int Char->Bool
areTreesEqual (Leaf x) (Leaf y) = x==y
areTreesEqual (Leaf x) tree = False
areTreesEqual tree (Leaf y) = False
areTreesEqual (Node x tx1 tx2) (Node y ty1 ty2) = if x==y
                                                  then (areTreesEqual tx1 ty1) && (areTreesEqual tx2 ty2)
                                                  else False

isSubtree::Tree Int Char->Tree Int Char->Bool
isSubtree (Leaf x) (Leaf y) = x==y
isSubtree (Node x t1 t2) (Leaf y) = False
isSubtree tree (Node x t1 t2) = if (areTreesEqual tree (Node x t1 t2))
                                then True
                                else (isSubtree tree t1) || (isSubtree tree t2)


-- Question 3 --
-- Section a --
rm_dups_rec::[Int]->[Int]
rm_dups_rec []=[]
rm_dups_rec [x]=[x]
rm_dups_rec (x:y:xs)=if x==y
                     then (rm_dups_rec (x:xs))
                     else (x:rm_dups_rec (y:xs))

-- Section b --
rm_elem::(Int->[Int]->[Int])
rm_elem x []=[x]
rm_elem x xs=if x==(head xs)
             then xs
             else (x:xs)

rm_dups::[Int]->[Int]
rm_dups=foldr (rm_elem) []

-- Section c --
rm_elem_str::(Char->String->String)
rm_elem_str x []=[x]
rm_elem_str x xs=if x==(head xs)
             then xs
             else (x:xs)

rm_dups_str::String->String
rm_dups_str=foldr (rm_elem_str) []

isPangram::String->Bool
isPangram str=(length (rm_dups_str (sort (map (toUpper) (filter (isAlpha) str)))))==26

-- Question 4 --
data BinTree a = Empty | BNode (BinTree a) a (BinTree a) deriving (Show)
-- Section a --
infTree :: a -> BinTree a
infTree x=(BNode (infTree x) x (infTree x))

-- Section b --
type Depth = Int
treeTake :: Depth -> BinTree a -> BinTree a
treeTake 0 tree=Empty
treeTake x (Empty)=Empty
treeTake x (BNode (leftTree) root (rightTree))=(BNode (treeTake (x-1) leftTree) root (treeTake (x-1) rightTree))

-- Section c --
treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f (BNode leftTree x rightTree) = (BNode (treeMap f leftTree) (f x) (treeMap f rightTree))
treeMap f Empty = Empty

-- Section d --
-- flatten $ treeTake 3 $ treeMap (+6) (infTree 2) = [8,8,8,8,8,8,8]
