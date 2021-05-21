{-# LANGUAGE DeriveGeneric #-}

module ULC where

import Data.Maybe (isJust)
import Debug.Trace (trace)
import Text.Printf (printf)
import GHC.Generics (Generic)
import Control.DeepSeq

-- https://www.researchgate.net/publication/312013355_Compiling_untyped_lambda_calculus_to_lower-level_code_by_game_semantics_and_partial_evaluation_invited_paper

data ULC = Var Int | Lam ULC | App ULC ULC
    deriving (Show, Eq, Generic)

instance NFData ULC

-- reduction semantics 1: classical substitution-based normal order beta reduction
semantic1 = snf where
    -- add x lvl e -> offset free variable by x
    add x lvl (Var i) = if i > lvl then Var (i + x) else Var i
    add x lvl (Lam e) = Lam $ add x (lvl + 1) e
    add x lvl (App e1 e2) = App (add x lvl e1) (add x lvl e2)
    -- subst e x e2 -> expression where index x in e is replaced with e2
    subst (Var i) x e2 = case i `compare` x of
        LT -> Var i
        EQ -> add (x - 1) 0 e2
        GT -> Var $ i - 1
    subst (Lam e) x e2 = Lam $ subst e (x + 1) e2
    subst (App e1' e2') x e2 = App (subst e1' x e2) (subst e2' x e2)
    -- wnf e -> weak normal form
    wnf x@(Var _) = x
    wnf x@(Lam _) = x
    wnf (App e1 e2) = case wnf e1 of
        Lam e -> wnf $ subst e 1 e2
        e -> App e e2
    -- snf e -> strong normal form
    snf x@(Var _) = x
    snf (Lam e) = Lam $ snf e
    snf (App e1 e2) = case wnf e1 of
        Lam e -> snf $ subst e 1 e2
        e1' -> App (snf e1') (snf e2)

-- reduction semantics 1-1: small change to semantics 1
semantic1_1 e = reduce e False where
    -- add x lvl e -> offset free variable by x
    add x lvl (Var i) = if i > lvl then Var (i + x) else Var i
    add x lvl (Lam e) = Lam $ add x (lvl + 1) e
    add x lvl (App e1 e2) = App (add x lvl e1) (add x lvl e2)
    -- subst e x e2 -> expression where index x in e is replaced with e2
    subst (Var i) x e2 = case i `compare` x of
        LT -> Var i
        EQ -> add (x - 1) 0 e2
        GT -> Var $ i - 1
    subst (Lam e) x e2 = Lam $ subst e (x + 1) e2
    subst (App e1' e2') x e2 = App (subst e1' x e2) (subst e2' x e2)
    -- reduce e bool -> find weak(T) or strong(F) normal form of e
    reduce x@(Var _) _ = x
    reduce x@(Lam _) True = x
    reduce (Lam e) False = Lam $ reduce e False
    reduce (App e1 e2) b = case reduce e1 True of
        Lam e -> reduce (subst e 1 e2) b
        e1' -> App (if b then e1' else reduce e1' False) (if b then e2 else reduce e2 False)

-- reduction semantics 1-2: small change to semantics 1-1
semantic1_2 e = reduce e False where
    -- add x lvl e -> offset free variable by x
    add x lvl (Var i) = if i > lvl then Var (i + x) else Var i
    add x lvl (Lam e) = Lam $ add x (lvl + 1) e
    add x lvl (App e1 e2) = App (add x lvl e1) (add x lvl e2)
    -- subst e x e2 -> expression where index x in e is replaced with e2
    subst (Var i) x e2 = case i `compare` x of
        LT -> Var i
        EQ -> add (x - 1) 0 e2
        GT -> Var $ i - 1
    subst (Lam e) x e2 = Lam $ subst e (x + 1) e2
    subst (App e1' e2') x e2 = App (subst e1' x e2) (subst e2' x e2)
    -- reduce e bool -> find weak(T) or strong(F) normal form of e
    reduce x@(Var _) _ = x
    reduce x@(Lam _) True = x
    reduce (Lam e) False = Lam $ reduce e False
    reduce (App e1 e2) b = case reduce e1 True of
        Lam e -> reduce (subst e 1 e2) b
        e1' -> App e1' (reduce e2 False)

-- reduction semantics 1-3: large change to semantics 1-2; lifting "subst"
data Sem2Env = Sem2Env [Maybe (ULC, Sem2Env)]
-- currently incorrect!!
semantic1_3 e = fst $ reduceDebug e False env0 where
    env0 :: Sem2Env
    env0 = let inner = Nothing : inner in Sem2Env inner
    -- add x lvl e -> offset free variable by x
    add x lvl (Var i) = if i > lvl then Var (i + x) else Var i
    add x lvl (Lam e) = Lam $ add x (lvl + 1) e
    add x lvl (App e1 e2) = App (add x lvl e1) (add x lvl e2)
    -- adjust i env -> adjust for substitution
    adjust i env = i - (length $ filter isJust $ take i env)
    -- reduce e flag env
    reduce (Var i) flag (Sem2Env env) = case env !! (i-1) of
        Just (e', env') -> reduce (add (i-1) 0 e') flag env'
        Nothing -> (Var (adjust i env), env0)
    reduce x@(Lam _) True env = (x, env)
    reduce (Lam e) False (Sem2Env env) = let (e', env') = reduce e False (Sem2Env $ Nothing : env) in (Lam e', env')
    reduce (App e1 e2) flag env = let (e1', Sem2Env env') = reduce e1 True env in
        case e1' of
            Lam e0 -> reduce e0 flag (Sem2Env $ Just (e2, env) : env')
            v -> let (e2', env'') = reduce e2 False env in (App v e2', env'')
    -- debug features
    summaryEnv (Sem2Env env) = (take 10 $ map (\x -> if isJust x then 'T' else 'F') env) ++ "..."
    reduceDebug e flag env = trace (printf "e=%s flag=%s env=%s" (show e) (show flag) (summaryEnv env)) $ reduce' e flag env
    reduce' (Var i) flag (Sem2Env env) = case env !! (i-1) of
        Just (e', env') -> reduceDebug (add (i-1) 0 e') flag env'
        Nothing -> (Var (adjust i env), env0)
    reduce' x@(Lam _) True env = (x, env)
    reduce' (Lam e) False (Sem2Env env) = let (e', env') = reduceDebug e False (Sem2Env $ Nothing : env) in (Lam e', env')
    reduce' (App e1 e2) flag env = let (e1', Sem2Env env') = reduceDebug e1 True env in
        case e1' of
            Lam e0 -> reduceDebug e0 flag (Sem2Env $ Just (e2, env) : env')
            v -> let (e2', env'') = reduceDebug e2 False env in (App v e2', env'')

-- reduction semantics 2: an attempted fix of 1-3 using environment increment, seems to work
data EnvItem = Bound ULC Env | Free Int
type Env = [EnvItem]
semantic2 e = fst $ reduce e False env0 where
    -- default environment
    env0 = map Free [1..]
    -- increment environment
    envIncr env = map envItemIncr env where
        envItemIncr (Free i) = Free (i + 1)
        envItemIncr (Bound e env') = Bound e $ envIncr env'
    -- reduce e flag env
    reduce (Var i) flag env = case env !! (i-1) of
        Bound e' env' -> reduce e' flag env'
        Free j -> (Var j, env0)
    reduce x@(Lam _) True env = (x, env)
    reduce (Lam e) False env = let (e', env') = reduce e False (Free 1 : envIncr env) in (Lam e', env')
    reduce (App e1 e2) flag env = let (e1', env') = reduce e1 True env in
        case e1' of
            Lam e0 -> reduce e0 flag (Bound e2 env : env')
            v -> let (e2', env'') = reduce e2 False env in (App v e2', env'')

-- reduction semantics 2-1: refactor of semantics 2 towards semantics 3
data AppCtx = Kend (ULC -> ULC) | Kapp ULC Bool Env (ULC -> ULC) AppCtx
semantic2_1 e = reduce e False env0 (Kend id) where
    -- chain a transformation to the head of ctx
    transform f (Kend g) = Kend (g . f)
    transform f (Kapp e b env g rest) = Kapp e b env (g . f) rest
    -- default environment
    env0 = map Free [1..]
    -- increment environment
    envIncr env = map envItemIncr env where
        envItemIncr (Free i) = Free (i + 1)
        envItemIncr (Bound e env') = Bound e $ envIncr env'
    -- reduce e flag env cont
    reduce :: ULC -> Bool -> Env -> AppCtx -> ULC
    reduce x@(Var i) flag env cont = case env !! (i-1) of
        Bound e' env' -> reduce e' flag env' cont
        Free j -> apk x cont env0 (Var j)
    reduce x@(Lam _) True env cont = apk x cont env x
    reduce (Lam e) False env cont = reduce e False (Free 1 : envIncr env) (transform Lam cont)
    reduce (App e1 e2) flag env cont = reduce e1 True env (Kapp e2 flag env id cont)
    -- apk e cont env e' (e = subexpression of original input, e' = subexpression of output)
    apk e (Kend f) env e' = f e'
    apk e (Kapp e' flag env' f cont) env e2 = case f e2 of
        Lam e'' -> reduce e'' flag (Bound e' env' : env) cont
        v -> reduce e' False env' (transform (App v) cont)

-- reduction semantics 2-2: minor refactor of semantics 2-1
semantic2_2 e = reduce e False env0 (Kend id) where
    -- chain a transformation to the head of ctx
    transform f (Kend g) = Kend (g . f)
    transform f (Kapp e b env g rest) = Kapp e b env (g . f) rest
    -- default environment
    env0 = map Free [1..]
    -- increment environment
    envIncr env = map envItemIncr env where
        envItemIncr (Free i) = Free (i + 1)
        envItemIncr (Bound e env') = Bound e $ envIncr env'
    -- reduce e flag env cont
    reduce :: ULC -> Bool -> Env -> AppCtx -> ULC
    reduce x@(Var i) flag env cont = case env !! (i-1) of
        Bound e' env' -> reduce e' flag env' cont
        Free j -> apk (Var j) cont env0
    reduce x@(Lam _) True env cont = apk x cont env
    reduce (Lam e) False env cont = reduce e False (Free 1 : envIncr env) (transform Lam cont)
    reduce (App e1 e2) flag env cont = reduce e1 True env (Kapp e2 flag env id cont)
    -- apk e cont env
    apk e (Kend f) env = f e
    apk e (Kapp e' flag env' f cont) env = case f e of
        Lam e'' -> reduce e'' flag (Bound e' env' : env) cont
        v -> reduce e' False env' (transform (App v) cont)

-- reduction semantics 3: refactor of semantics 2-2 so that environment does not depend on infinite list
-- and normal form construction does not depend on function composition
-- so that the algorithm is portable to imperative languages
data EnvItem' = Bound' ULC Env' Int | Free' Int
type Env' = [EnvItem']
data Tok = LamTok | AppTok ULC
data AppCtx' = Kend' [Tok] | Kapp' ULC Bool Env' [Tok] AppCtx'
semantic3 e = reduce e False env0 (Kend' []) where
    -- chain a transformation to the head of ctx
    transform f (Kend' g) = Kend' (f : g)
    transform f (Kapp' e b env g rest) = Kapp' e b env (f : g) rest
    -- default environment; it is assumed to be filled with Free' 1
    env0 = []
    -- increment environment
    envOffset [] x = [Free' (1 + x)]
    envOffset (Bound' e env i : t) x = Bound' e env (i + x) : t
    envOffset (Free' i : t) x = Free' (i + x) : t
    -- extract item from env
    extract env i = extract' env i 0 where
        extract' [] i j = Free' (i + j)
        extract' (Free' x : t) 1 j = Free' (x + j)
        extract' (Free' x : t) i j = extract' t (i - 1) (x + j)
        extract' (Bound' e env k : t) 1 j = Bound' e env (k + j)
        extract' (Bound' e env k : t) i j = extract' t (i - 1) (k + j)
    -- realize a token list
    realize [] e = e
    realize (LamTok : t) e = realize t (Lam e)
    realize (AppTok e' : t) e = realize t (App e' e)
    -- reduce e flag env cont
    reduce :: ULC -> Bool -> Env' -> AppCtx' -> ULC
    reduce x@(Var i) flag env cont = case extract env i of
        Bound' e' env' j -> reduce e' flag (envOffset env' j) cont
        Free' j -> apk (Var j) cont env0
    reduce x@(Lam _) True env cont = apk x cont env
    reduce (Lam e) False env cont = reduce e False (Free' 1 : env) (transform LamTok cont)
    reduce (App e1 e2) flag env cont = reduce e1 True env (Kapp' e2 flag env [] cont)
    -- apk e cont env
    apk e (Kend' f) env = realize f e
    apk e (Kapp' e' flag env' f cont) env = case realize f e of
        Lam e'' -> reduce e'' flag (Bound' e' env' 0 : env) cont
        v -> reduce e' False env' (transform (AppTok v) cont)
