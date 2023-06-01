data Expr = C Int |
            Comb Expr Expr| 
            V String |
            Let String Expr Expr


eval :: Expr -> [(String, Int)] -> ([(String, Int)], Int)
eval (C x)  env       = (env, x)
eval (Comb e1 e2) env   =  let env1 = fst (eval e1 env)
                               v1 = snd (eval e1 env)
                               env2 = fst (eval e2 env1)
                               v2 = snd (eval e2 env1)
                            in (env2, v1 + v2)

-- eval (V v)  env       = (env, find v env)
eval (Let v e1 e2) env = let env1 = fst (eval e1 env)
                             v1 = snd (eval e1 env)
                             env2       = extend v v1  
                             ans = eval e2 env2
                         in  ans

---Find a variable's value by looking in the environment
-- find v  []          = undefined
-- find v1 ((v2,e):es) = if v1 == v2 then e else find v1 es
                     
extend :: String -> Int -> [(String, Int)] -> [(String, Int)]
extend v e env  = (v,e):env

answer :: Expr -> ([(String, Int)], Int)
answer e = eval e []

-- theme: function, builtin
-- goanna results: 4
-- oracle: true
-- intended fix: 3
-- response time: 1.3374247550964355
-- mus size: 4
-- ghc loc: 1
-- ghc diagnosis: 1
-- ghc fix: 0
-- goanna1: 1
-- goanna2: 2
-- goanna3: 3
-- goanna4: 4
-- goanna5: 4
