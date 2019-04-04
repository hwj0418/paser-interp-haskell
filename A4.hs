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

-- Lecture code
mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

-- Lecture code
-- "type error" -> TypeError
intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

-- New code
-- boolOrDie -> supports VB aka boolean type
-- Modified from intOrDie
boolOrDie :: Value -> Either Error Bool
boolOrDie (VB a) = pure a
boolOrDie _ = Left TypeError

-- Lecture code
interp :: Map String Value -> Expr -> Either Error Value
interp _ (Num i) = pure (VN i)
interp _ (Bln b) = pure (VB b)

-- Lecture code
-- "variable not found" -> VarNotFound
interp env (Var v) = case Map.lookup v env of
  Just a -> pure a
  Nothing -> Left VarNotFound

-- Lecture code
-- Plus operation for integers
interp env (Prim2 Plus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i + j))

-- New code
-- Minus operation
-- Modified from lecture code for plus operation
interp env (Prim2 Minus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i - j))

-- Lecture code
-- Multiplication operation for integers
interp env (Prim2 Mul e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i * j))

-- New code
-- Divide operation
-- Modified from lecture code for multiplication operation
-- Add error handling for dived by 0 case
interp env (Prim2 Div e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    if j == 0
        then Left DivByZero
        else pure (VN (i `div` j))

-- New code
-- Mod operation
-- Modified from lecture code for multiplication operation
-- Add error handling for dived by 0 case
interp env (Prim2 Mod e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    if j == 0
        then Left DivByZero
        else pure (VN (i `mod` j))

-- On lecture note it saids: You have to support True==True too on your assignment.
-- But on assignment handout it saids: For simplicity, Eq and Lt work for integer operands only. If an operand is not an integer, it is a type error.

-- Lecture code
-- Add type error handling
interp env (Prim2 Eq e1 e2) = do
    a <- interp env e1
    case a of 
        -- For integer type
        VN i -> do
            i <- intOrDie a
            b <- interp env e2
            j <- intOrDie b
            pure (VB (i == j))
        -- Any other type will be considered as TypeError.
        _ -> Left TypeError

-- New code
-- Lessthan comparison
-- Modified from Eq comparison
-- Add type error handling
interp env (Prim2 Lt e1 e2) = do
    a <- interp env e1
    case a of 
        -- Compare integers
        VN i -> do
            b <- interp env e2
            j <- intOrDie b
            pure (VB (i < j))
        -- Any other type will be considered as TypeError.
        _ -> Left TypeError

-- Lecture code
-- "type error" -> TypeError
interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
      VB True -> interp env eThen
      VB False -> interp env eElse
      _ -> Left TypeError

-- Lecture code (partial code was comment out)
interp env (Let eqns evalMe) = do
    env' <- extend env eqns
    interp env' evalMe
  where
    extend env [] = pure env
    extend env ((v,rhs) : eqns) = do
        a <- interp env rhs
        let env' = Map.insert v a env
        extend env' eqns

-- Lecture code
interp env (Lambda v body) = pure (VClosure env v body)

-- Lecture code
-- "wrong type" -> TypeError
interp env (App f e) = do
    c <- interp env f
    case c of
      VClosure fEnv v body -> do
          eVal <- interp env e
          let bEnv = Map.insert v eVal fEnv
          interp bEnv body
      _ -> Left TypeError
