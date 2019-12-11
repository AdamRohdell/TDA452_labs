import Data.List
import Parsing
import Data.Char
import Data.Maybe

data Expr = Num Double
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq, Show)

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


expr, term, factor :: Parser Expr
expr = foldl1 Add <$> chain term (char '+')
term = foldl1 Mul <$> chain factor (char '*')
factor = Num <$> number <|> do char '(' *> expr <* char ')' 
                <|> Sin <$> sinus <|> Cos <$> cosinus

number :: Parser Double
number = read <$> oneOrMore digit

addition :: Parser Double
addition = operation '+' (+)

multiplication :: Parser Double
multiplication = operation '*' (*)

sinus :: Parser Expr
sinus = do
        char 's'
        char 'i'
        char 'n'
        char '('
        n <- expr
        char ')'
        return n

cosinus :: Parser Expr
cosinus = do
        char 'c'
        char 'o'
        char 's'
        char '('
        n <- expr
        char ')'
        return n

operation :: Char -> (Double -> Double -> b) -> Parser b
operation c op = do
        n <- number
        char c --sat (=='+') s
        m <- number
        return (m `op` n)
        
calculation :: Parser Double
calculation = addition <|> multiplication

{-
num' :: String -> Maybe(Int, String)
num' str = case span isDigit str of 
            ((c:cs),rest) -> Just (read (c:cs), rest)
            _             -> Nothing


additionParse :: String -> Maybe(Int, String)
additionParse n = case num' n of 
            Just (d, '+':r) -> case num' r of
                                    Just (m, r') -> Just(d+m, r')
                                    Nothing      -> Nothing
            _               -> Nothing
-}
{-readExpr :: String -> Maybe Expr
readExpr str = parse parser str

            
            where parser = Parser Num
    -}


readEvalPrint = do
        putStr "What would you like to calculate?"
        s <- getLine
        let s' = filter (not.isSpace) s
        case parse expr s' of
                Just (ans,"") -> print (eval ans 0)
                _             -> putStrLn "invalid expression"

