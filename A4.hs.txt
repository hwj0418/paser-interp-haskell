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
mainParser = error "TODO"


mainInterp :: Expr -> Either Error Value
mainInterp = error "TODO"
