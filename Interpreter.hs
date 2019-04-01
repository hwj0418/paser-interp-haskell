module A5Interpreter where

    import           Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map
    -- You can add more imports.
    
    import           A5Term
    
    mainInterp :: Term -> Value
    mainInterp = interp Map.empty
    
    interp :: Map String Value -> Term -> Value
    interp env (Var v) = case Map.lookup v env of
      Nothing -> error ("variable " ++ v ++ " not found")
      Just val -> val
    interp env (Num i) = VN i
    interp env (Bln b) = VB b
    interp env (Neg e) = case interp env e of
      VN i -> VN (- i)
    interp env (Prim2 And e1 e2) = case interp env e1 of
      v@(VB False) -> v
      VB True -> case interp env e2 of v@(VB _) -> v
    interp env (Prim2 Or e1 e2) = case interp env e1 of
      v@(VB True) -> v
      VB False -> case interp env e2 of v@(VB _) -> v
    interp env (Prim2 Eq e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VB (i == j)
          (VB b, VB c) -> VB (b == c)
    interp env (Prim2 Neq e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VB (i /= j)
          (VB b, VB c) -> VB (b /= c)
    interp env (Prim2 Lt e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VB (i < j)
    interp env (Prim2 Leq e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VB (i <= j)
    interp env (Prim2 Plus e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VN (i + j)
    interp env (Prim2 Minus e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VN (i - j)
    interp env (Prim2 Mul e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) -> VN (i * j)
    interp env (Prim2 Div e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) | j /= 0 -> VN (div i j)
    interp env (Prim2 Mod e1 e2) =
        case (interp env e1, interp env e2) of
          (VN i, VN j) | j /= 0 -> VN (mod i j)
    interp env (Cond t e1 e2) = case interp env t of
      VB True -> interp env e1
      VB False -> interp env e2
    interp env (Lambda var body) = VClosure env var body
    interp env (Let [] body) = interp env body
    interp env (Let ((v,rhs) : defs) body) =
        let a = interp env rhs
            new_env = Map.insert v a env
        in interp new_env (Let defs body)
    interp env (App f e) = case interp env f of
      VClosure fEnv v body ->
          let eVal = interp env e
              bEnv = Map.insert v eVal fEnv
          in interp bEnv body