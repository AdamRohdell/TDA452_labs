import Data.List
import Parsing


data Expr = Num Double
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq)

type Name = String

sin' = Prelude.sin
cos' = Prelude.cos

x :: Expr
x = Var "x"

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
showExpr (Var s)     = s
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




readExpr :: String -> Maybe Expr
readExpr str = parse parser str

            
            where parser = Parser Num
    



