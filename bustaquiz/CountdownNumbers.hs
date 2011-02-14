module CountdownNumbers (
  ) where

import Logic
import Data.List

largeNumbers :: [Int]
largeNumbers = [25,50,75,100]

smallNumbers :: [Int]
smallNumbers = [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]

selectNumbers :: Int -> Int -> [Int]
selectNumbers seed large = rndSelect seed largeNumbers large ++ rndSelect seed smallNumbers (6 - large)

genResult :: Int -> [Int] -> Result
genResult seed options = outputResult where
    chosen = rndSelect seed options 2
    firstResult = generateResult seed chosen
    remaining = options\\chosen
    outputResult = possiblyCombine seed firstResult remaining
    
possiblyCombine :: Int -> Result -> [Int] -> Result
possiblyCombine seed firstResult remaining =
    case remaining of
        [] -> firstResult
        [x] -> firstResult
        (x:y:xs) -> chooseFromList seed (combine' firstResult (genResult (myKingdomForASeed seed) remaining))    
    
generateResult :: Int -> [Int] -> Result
generateResult seed options = 
    let leftInt = head options
        rightInt = last options
        left = (Val leftInt)
        right = (Val rightInt)
    in chooseFromList seed (combine' (left, leftInt) (right, rightInt))

data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x >= y
valid Div x y = y /= 1 && x `mod` y == 0  

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

show' :: Op -> String
show' Add = "+"
show' Sub = "-"
show' Mul = "*"
show' Div = "/"

data Expr = Val Int | App Op Expr Expr

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y] 

--Some helper stuff to read out the expressions
getReadableSolutions :: [Expr] -> [String]
getReadableSolutions = map printExpr

printExpr :: Expr -> String
printExpr (Val x) = show x
printExpr (App o l r) = "(" ++ printExpr l ++ " " ++ show' o ++ " " ++ printExpr r ++ ")"

printResult :: Result -> String
printResult (Val x, _) = show x
printResult (App o l r, y) = "(" ++ printExpr l ++ " " ++ show' o ++ " " ++ printExpr r ++ ")" ++ "=" ++ show y


--This is to get the solution for a randomly generated case so we don't need it (it is too expensive) Interesting to keep around though
--http://www.cs.nott.ac.uk/~gmh/countdown.pdf
subbags :: (Eq a) => [a] -> [[a]]
subbags xs = [zs | ys <- subs xs, zs <- perms ys]

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms ( xs\\[x] ) ]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

solution :: Expr -> [Int] -> Int -> Bool
solution e options target = elem (values e) (subbags options) && eval e == [target]

split  :: [a] -> [([a],[a])]
split [] = [([],[])]
split (x:xs) = ([], x:xs):[(x:ls, rs) | (ls,rs) <- split xs]

nesplit :: [a] -> [([a],[a])]
nesplit xs = filter ne (split xs)

ne :: ([a],[b]) -> Bool
ne (xs, ys) = not (null xs || null ys)

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- nesplit ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions options target = [e | options' <- subbags options, e <- exprs options', eval e == [target]]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- nesplit ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' options target = [e | options' <- subbags options, (e, m) <- results options', m == target]