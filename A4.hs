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
mainParser = whitespaces *> expr <* eof

expr :: Parser Expr
expr = or
    where
        -- use chainr to pass concat operation
        or = chainr1 cat (char '+' *> whitespaces *> pure (Or))
        -- similarly, use chainr to pass star operation
        cat = chainr1 star (whitespaces *> pure (Cat))
        star = do
            x <- atom
            y <- whitespaces
            z <- many (char '*')
            pure (foldr (\_ a -> Star a) x z)  
        atom = bit <|>  between (char '(' *> whitespaces)
                                (char ')' *> whitespaces)
                                expr
        -- bit function only allows 0 and 1 upon the bit
        bit = A <$> b <$> (satisfy (== '0') <|> satisfy (== '1'))
        b n
        |(n == '0') = B0
        |(n == '1') = B1


mainInterp :: Expr -> Either Error Value
mainInterp = error "TODO"

cond = do
    keyword "if"
    ifCase <- block
    keyword "then"
    thenCase <- block
    keyword "else"
    elseCase <- block
    return (Cond myTest myThen myElse)

lambda = do
    operator "\\"
    v <- var
    operator "->"
    b <- block
    return (Lambda v b)

myLet = do 
    keyword "let"
    eqns <- many equation
    keyword "in"
    body <- block
    return (Let eqns body)

infix = do 
    a <- arith
    (do c <- cmp
        b <- arith
        return (Prim2 c a b))
        <|> return a