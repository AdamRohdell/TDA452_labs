module Expr where

import Data.List
import Parsing
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import Data.String

data Expr = Num Double
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq, Show)

instance Arbitrary Expr where
    arbitrary = sized arbExpr
        

type Name = Char  

sin' = Prelude.sin
cos' = Prelude.cos

x :: Expr
x = Var 'x'

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Add 
mul = Mul

sin,cos :: Expr -> Expr
sin = Sin
cos = Cos 



showExpr :: Expr -> String
showExpr (Num d)     = show d
showExpr (Var s)     = s:""
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showExpr e1 ++ "*" ++ showExpr e2
showExpr (Sin e)     = "sin" ++ showExpr e
showExpr (Cos e)     = "cos" ++ showExpr e



eval :: Expr -> Double -> Double
eval (Num d) c     = d
eval (Var s) d     = d
eval (Add e1 e2) d = eval e1 d + eval e2 d
eval (Mul e1 e2) d = eval e1 d * eval e2 d
eval (Sin e) d     = sin' (eval e d)
eval (Cos e) d     = cos' (eval e d)


expr, term, factor :: Parser Expr
expr = foldl1 Add <$> chain term (char '+')
term = foldl1 Mul <$> chain factor (char '*')
factor = Num <$> number <|> do char '(' *> expr <* char ')' 
                <|> Sin <$> sinus <|> Cos <$> cosinus <|> Var <$> char 'x'



number :: Parser Double
number = read <$> oneOrMore (digit <|> char '.')

addition :: Parser Double
addition = operation '+' (+)

multiplication :: Parser Double
multiplication = operation '*' (*)

sinus :: Parser Expr
sinus = do
        mapM_ char "sin"
        n <- expr
        return n

cosinus :: Parser Expr
cosinus = do
        mapM_ char "cos"
        n <- expr
        return n

operation :: Char -> (Double -> Double -> b) -> Parser b
operation c op = do
        n <- number
        char c 
        m <- number
        return (m `op` n)
        
calculation :: Parser Double
calculation = addition <|> multiplication

readExpr :: String -> Maybe Expr
readExpr str = case parse expr (filter (not.isSpace) str) of
                Just(expression, rest) -> Just expression
                mapM_                  -> Nothing
                


prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e1
                |  isNothing $ readExpr(showExpr e1) = False
                |  readExpr(showExpr e1) == Just e1  = True
                |  otherwise                         = False


arbExpr :: Int -> Gen Expr
arbExpr n = arbExpr'

arbExpr' :: Gen Expr
arbExpr' = frequency [(6, arbitrary), (2, arbBin), (1, arbFun), (1, return (Var 'x'))]

arbFun :: Gen Expr 
arbFun = do
        f <- elements [Sin, Cos]
        e <- arbExpr'
        return $ f e

arbBin :: Gen Expr
arbBin = do
        n1 <- arbNum
        n2 <- arbNum
        frequency [(5, return (Mul n1 n2)), (5, return (Add n1 n2))]

arbNum :: Gen Expr
arbNum = Num <$> suchThat arbitrary (>0.1)
        
simplify :: Expr -> Expr
simplify (Mul (Num 0) _)         = Num 0
simplify (Mul _ (Num 0))         = Num 0

simplify (Add (Num n1) (Num n2)) = Num (n1+n2)
simplify (Mul (Num n1) (Num n2)) = Num (n1*n2)

simplify (Sin e)                 = Sin (simplify e)
simplify (Cos e)                 = Cos (simplify e)

simplify (Add e1 (Num 0))        = simplify e1
simplify (Add (Num 0) e1)        = simplify e1
simplify (Mul (Num 1) e1)        = simplify e1
simplify (Mul e1 (Num 1))        = simplify e1

simplify (Add e1 e2)
        | e1 /= e1' && e2 /=e2' = simplify $ Add (simplify e1) (simplify e2)
        | otherwise             = (Add (simplify e1) (simplify e2))
                where   e1' = simplify e1
                        e2' = simplify e2

simplify (Mul e1 e2)  
        | e1 /= e1' && e2 /=e2' = simplify $ Mul (simplify e1) (simplify e2)
        | otherwise             = Mul (simplify e1) (simplify e2)
                where   e1' = simplify e1
                        e2' = simplify e2  
simplify e                       = e
        

prop_simplify e d = eval e d == eval (simplify e) d

differentiate :: Expr -> Expr
differentiate (Num n)                   = Num 0
differentiate (Var 'x')                 = Num 1
differentiate (Mul e1 (Var 'x'))        = simplify e1
differentiate (Add e1 e2)               = simplify (Add (differentiate e1) (differentiate e2))    --Sumregeln
differentiate (Mul e1 e2)               = simplify (Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2))) --Kedjeregeln
differentiate (Sin e1)                  = simplify (Mul (Cos e1) (differentiate e1))                                      
differentiate (Cos e1)                  = simplify (Mul (Mul (Sin e1) (Num (-1))) (differentiate e1))


