module Expr where

import Data.List
import Parsing
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import Data.String

data Operator = Add|Mul
        deriving (Eq)
instance Show Operator where
        show Add = "Add"
        show Mul = "Mul" 

type FunctionName = String
        

data Expr = Num Double
          | Var Char
          | Operation Operator Expr Expr
          | Function FunctionName Expr
          deriving  (Show)

instance Eq Expr where
        (==) e1 e2 = abs((eval e1 1) - (eval e2 1)) < 0.000001

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
add = Operation Add 
mul = Operation Mul

sin,cos :: Expr -> Expr
sin = Function "Sin"
cos = Function "Cos"



showExpr :: Expr -> String
showExpr (Num d)     = show d
showExpr (Var s)     = s:""
showExpr (Operation Mul e1 e2) = showFactor e1 ++ showOp Mul ++ showFactor e2
showExpr (Operation op e1 e2) = showExpr e1 ++ showOp op ++ showExpr e2
showExpr (Function f e)     = showFun (Function f e) ++ "(" ++ showExpr e ++ ")"

showFactor :: Expr -> String
showFactor (Num n) = show n
showFactor e = "(" ++ showExpr e ++ ")"

showOp :: Operator -> String
showOp Mul = "*"
showOp Add = "+"

showFun :: Expr -> String
showFun (Function "Cos" e) = "Cos "
showFun (Function "Sin" e) = "Sin "


evalOp :: Operator -> (Double -> Double -> Double)
evalOp Add = (+)
evalOp Mul = (*)

evalFun :: String -> (Double -> Double)
evalFun "Sin" = sin'
evalFun "Cos" = cos'

eval :: Expr -> Double -> Double
eval (Num d) c              = d
eval (Var s) d              = d
eval (Operation op e1 e2) d = evalOp op (eval e1 d) (eval e2 d)
eval (Function f e) d       = evalFun f (eval e d)


expr, term, factor :: Parser Expr
expr = foldl1 (Operation Add) <$> chain term (char '+')
term = foldl1 (Operation Mul) <$> chain factor (char '*')
factor = Num <$> number <|> do char '(' *> expr <* char ')' 
                <|> Function "Sin" <$> mathFunc "Sin" <|> Function "Cos" <$> mathFunc "Cos" <|> Var <$> char 'x'

number :: Parser Double
number = read <$> oneOrMore (digit <|> char '.')

addition :: Parser Double
addition = operation '+' (+)

multiplication :: Parser Double
multiplication = operation '*' (*)

mathFunc :: FunctionName -> Parser Expr
mathFunc f = do 
             mapM_ char f
             parsingWithParenthesis <|> Var <$> char 'x' <|> Num <$> number

parsingWithParenthesis :: Parser Expr
parsingWithParenthesis = do
                         char '('
                         n <- expr
                         char ')'
                         return n

operation :: Char -> (Double -> Double -> b) -> Parser b
operation c op = do
        n <- number
        char c
        m <- number
        return (m `op` n)
        
calculation :: Parser Double
calculation = multiplication <|> addition

readExpr :: String -> Maybe Expr
readExpr str = case parse expr (filter (not.isSpace) str) of
                Just(expression, rest) -> Just expression
                mapM_                  -> Nothing
                


prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e =
        case readExpr (showExpr e) of
                Just a -> e == a
                Nothing -> error "bad input xd"


arbExpr :: Int -> Gen Expr
arbExpr 0 = arbNum
arbExpr n = frequency [(5, arbFun (arbExpr (n-1))), (4, do
                                                    m <- choose (0, n-1)
                                                    arbBin (arbExpr m) (arbExpr (n-1-m))),
                        (0, return (Var 'x'))]

arbFun :: Gen Expr -> Gen Expr
arbFun e = do e1 <- e
              elements [Function "Sin" e1, Function "Cos" e1]

arbBin :: Gen Expr -> Gen Expr -> Gen Expr
arbBin e1 e2 = do e1' <- e1
                  e2' <- e2
                  elements [Operation Add e1' e2', Operation Mul e1' e2']

arbNum :: Gen Expr
arbNum = Num <$> suchThat arbitrary (>0.1)

simplify' :: Expr -> Expr
simplify' (Operation Add (Operation Add (Var 'x') (Num n1)) (Num n2)) = 
                Operation Add (Var 'x') (Num (n1+n2))
simplify' (Operation Add (Operation Add (Num n1) (Var 'x')) (Num n2)) = 
                Operation Add (Var 'x') (Num (n1+n2))
simplify' (Operation Mul (Operation Mul (Var 'x') (Num n1)) (Num n2)) = 
                Operation Mul (Var 'x') (Num (n1*n2))
simplify' (Operation Mul (Operation Mul (Num n1) (Var 'x')) (Num n2)) = 
                Operation Mul (Var 'x') (Num (n1*n2))                
simplify' e = e


simplify :: Expr -> Expr
simplify (Operation Mul (Num 0) _)         = Num 0
simplify (Operation Mul _ (Num 0))         = Num 0

simplify (Operation Add (Num n1) (Num n2)) = Num (n1+n2)
simplify (Operation Mul (Num n1) (Num n2)) = Num (n1*n2)

simplify (Function f e)                    = Function f (simplify e)

simplify (Operation Add e1 (Num 0))        = simplify e1
simplify (Operation Add (Num 0) e1)        = simplify e1
simplify (Operation Mul (Num 1) e1)        = simplify e1
simplify (Operation Mul e1 (Num 1))        = simplify e1

simplify (Operation Add e1 e2)
        | e1 /= e1' || e2 /=e2' = simplify $ Operation Add (simplify e1) (simplify e2)
        | otherwise             = simplify' (Operation Add e1 e2)--Operation Add e1 e2
                where   e1' = simplify e1
                        e2' = simplify e2

simplify (Operation Mul e1 e2)  
        | e1 /= e1' || e2 /=e2' = simplify $ Operation Mul (simplify e1) (simplify e2)
        | otherwise             = simplify' (Operation Mul e1 e2)
                where   e1' = simplify e1
                        e2' = simplify e2  
simplify e                      = e
   
prop_simplify e d = 
        (eval e d == eval (simplify e) d)
        && simplify e == simplify (simplify e)

differentiate :: Expr -> Expr
differentiate (Num n)                   = Num 0
differentiate (Var 'x')                 = Num 1
differentiate (Operation Mul e1 (Var 'x'))        = simplify e1
differentiate (Operation Add e1 e2)               = simplify (Operation Add (differentiate e1) (differentiate e2))    --Sumregeln
differentiate (Operation Mul e1 e2)               = simplify (Operation Add (Operation Mul (differentiate e1) e2) (Operation Mul e1 (differentiate e2))) --Kedjeregeln
differentiate (Function "Sin" e1)                  = simplify (Operation Mul (Function "Cos" e1) (differentiate e1))                                      
differentiate (Function "Cos" e1)                  = simplify (Operation Mul (Operation Mul (Function "Sin" e1) (Num (-1))) (differentiate e1))


