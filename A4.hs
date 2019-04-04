module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = do
    whitespaces
    b <- block
    eof
    return b
-- "  let x \n     = 4 * ( \n  5+6) ; \n in \\ y ->  if  x< y then x else y\n"
-- let x = 4 * (5 + 6);
-- in \y -> if x < y then x else y
-- cond: if x < y then x else y 
block = cond <|> lambda <|> myLet <|> myInfix

mainInterp :: Expr -> Either Error Value
mainInterp = error "TODO"

cond = do
    terminal "if"
    ifCase <- block
    terminal "then"
    thenCase <- block
    terminal "else"
    elseCase <- block
    return (Cond ifCase thenCase elseCase)

lambda = do
    operator "\\"
    v <- var
    operator "->"
    b <- block
    return (Lambda v b)

myLet = do 
    terminal "let"
    equations <- some equation
    terminal "in"
    body <- block
    return (Let equations body)

equation = do
    v <- var
    terminal "="
    b <- block
    terminal ";"
    return (v, b)

myInfix = do
    a <- arith
    (do c <- cmp
        b <- arith
        return (Prim2 c a b))
     <|> return a

cmp = operator "==" *> pure Eq <|> operator "<" *> pure Lt

arith = chainl1 addend addop

addop = fmap Prim2 (operator "+" *> pure Plus <|> operator "-" *> pure Minus)

addend = chainl1 factor mulop

factor = fmap (foldl1 App) (some atom)

mulop = 
    fmap Prim2 (operator "*" *> pure Mul <|> operator "/" *> pure Div <|> operator "%" *> pure Mod)

atom = (do
    terminal "("
    b <- block
    terminal ")"
    return b) <|> literal <|> fmap Var var

literal = fmap Num natural <|> fmap Bln boolean

boolean = (keyword "True" *> pure True) <|> (keyword "False" *> pure False)

var = identifier ["if", "then", "else", "let", "in", "True", "False"]