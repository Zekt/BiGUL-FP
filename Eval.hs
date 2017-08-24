import Terms

eval :: Term a -> Env -> Val a
eval (Lit v) env = v
eval (Var n) env = env n
eval (Pair t1 t2) env = P (eval t1 env, eval t2 env)
eval (Cons t1 t2) env =
    case (eval t1 env, eval t2 env) of
      (v, L vs) -> L (v:vs)
      _ -> error "type error"
eval (Lam n _ t) env = F (\v -> eval t (extend n v env))
eval (App t1 t2) env =
    case (eval t1 env, eval t2 env) of
      ((F f), v) -> f v
      (_, _) -> error "type error"
eval (Put (Clause sp vp expr) s v) env =
    case match (eval s env) sp env of
      Just env' -> case match (eval v env') vp env' of
                     Just env'' -> eval expr env''
eval (Get (Clause sp vp expr) s) env = undefined


match :: Val -> Term -> Env -> Maybe Env
match (N nv) (Lit (N nt)) env = if nv == nt then Just env
                                            else Nothing
match (B bv) (Lit (B bt)) env = if bv == bt then Just env
                                            else Nothing
match v (Var n) env = Just (extend n v env)
match (P (v1, v2)) (Pair t1 t2) env =
    case match v1 t1 env of
      Just env' -> match v2 t2 env'
match (L (v:vs) typ) (Cons head tail) env =
    case match v head env of
      Just env' -> match (L vs typ) tail env'
match _ _ _ = Nothing
