module Unification where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Ast
import Debug.Trace (trace)

flipInOut :: Monad m => [m a] -> m [a]
flipInOut [] = return []
flipInOut (x:xs) = do
    x <- x
    xs <- flipInOut xs
    return $ x:xs



type Gamma =  [M.Map String Type]
type Subst =  M.Map Int Type



type InferState = ExceptT String (State (Subst, Int))

union :: Int -> Type -> InferState ()
union k t = do
    (subst, g) <- get
    let subst' = M.insert k t subst
    put (subst', g)

genTVar :: InferState Int
genTVar = do
    (s, tv) <- get
    put (s, tv + 1)
    return tv

inst :: Type -> InferState Type
inst = inst' S.empty where
    inst' s t@(TVar x) = if x `S.member` s then TVar <$> genTVar else return t
    inst' s (TFunc p r) = do
        p <- flipInOut $ fmap (inst' s) p
        r <- inst' s r
        return $ TFunc p r
    inst' s (Forall x b) = inst' (foldr S.insert s x) b
    inst' _ t = return t

unify :: Type -> Type -> InferState ()
unify a b = do
    a <- inst a
    b <- inst b
    unify' a b where
        unify' (TVar a) y = union a y
        unify' x (TVar b) = union b x
        unify' (TFunc ps1 r1) (TFunc ps2 r2) = unifyMany ps1 ps2 >> unify' r1 r2 where
            unifyMany [] [] = return ()
            unifyMany (x: xs) (y: ys) = unify x y >> unifyMany xs ys
            unifyMany _ _ = throwError "Arity not match"
        unify' (TCons a) (TCons b) = if a == b then return () else throwError $ "type mismatch " ++ show a ++ " " ++ show b
        unify' a b = throwError "type error"

find :: Type -> InferState Type
find = find' S.empty where
    find' :: S.Set Int -> Type -> InferState Type
    find' s t@(TVar v) = if v `S.member` s then throwError "cannot create infinite type" else do
        subst <- fst <$> get
        let s' = S.insert v s
        case  M.lookup v subst of
            Nothing -> return t
            Just t -> do
                t <- find' s' t
                union v t
                return t
    find' s (TFunc ps r) = do
        ps <- flipInOut $ fmap (find' s) ps
        r <- find' s r
        return $ TFunc ps r
    find' s (Forall xs t) = find' s t >>= return . Forall xs
    find' _ t = return t


lookupGamma :: String -> Gamma -> InferState Type
lookupGamma s [] = throwError $ "cannot find variable " ++ s
lookupGamma s (d:ds) = case M.lookup s d of
    Nothing -> lookupGamma s ds
    Just t -> return t

genTVars :: [String] -> InferState [(String, Type)]
genTVars [] = return []
genTVars (x:xs) = do
    t <- TVar <$> genTVar
    m <- genTVars xs
    return $ (x, t):m

infer :: Gamma -> Expr -> InferState (Type, Expr)
infer _ e@(Bool _) = return (boolType, e)
infer _ e@(Int _) = return (intType, e)
infer gamma e@(Var _ name) = do
    t <- lookupGamma name gamma >>= inst
    return (t, e)
infer gamma (Lamb _ xs body) = do
    ts <- genTVars xs
    let gamma' = trace ("trace1: " ++ show ts) $ M.fromList ts : gamma
    (tbody, body) <- trace "trace3: " infer gamma' body
    tparams <- flipInOut $ fmap (find . snd) ts
    let getBoundTVars _ [] = []
        getBoundTVars s (TVar x:xs) = 
            if x `elem` s 
            then getBoundTVars s xs
            else x : getBoundTVars (S.insert x s) xs
        getBoundTVars s (x:xs) = getBoundTVars s xs
    let boundTVars = getBoundTVars S.empty tparams
    let t = TFunc tparams tbody
    let t1 = case boundTVars of
                [] -> t
                _ -> Forall boundTVars t
    return (t1, Lamb t1 xs body)
infer gamma (App f args) = do
    (tf, f) <- infer gamma f
    (targs, args) <- unzip <$> (flipInOut $ fmap (infer gamma) args)
    tr <- TVar <$> genTVar
    unify tf (TFunc targs tr)
    tr <- find tr
    return (tr, App f args)
infer gamma (Let bindings body) = do
    let (texprs, names, exprs) = unzip3 bindings
    (texprs, exprs) <- fmap unzip $ flipInOut $ fmap (infer gamma) exprs
    let m = M.fromList $ zip names texprs
    (tbody, body) <- infer (m:gamma) body
    let bindings = zip3 texprs names exprs
    return (TFunc texprs tbody, Let bindings body)
infer gamma (Block defs expr) = do
    (m, defs) <- inferDefs gamma defs
    (texpr, expr) <- infer (m:gamma) expr
    return (texpr, Block defs expr)
infer gamma (If a b c) = do
    (ta, a) <- infer gamma a
    (tb, b) <- infer gamma b
    (tc, c) <- infer gamma c
    unify boolType ta
    unify tb tc
    t <- find tc
    return (t, If a b c)
infer gamma (UnOp op expr) = do
    (t, e) <- infer gamma expr
    let (te, tr) = primUnOpType op
    unify te t
    return (tr, UnOp op e)
infer gamma (BinOp op e1 e2) = do
    (t1, e1) <- infer gamma e1
    (t2, e2) <- infer gamma e2
    let ((ta, tb), tr) = primBinOpType op
    unify ta t1
    unify tb t2
    return (tr, BinOp op e1 e2)

primUnOpType "-" = (intType, intType)
primUnOpType "not" = (boolType, boolType)

primBinOpType x = 
    if x `elem` words "+ - * /"
    then ((intType, intType), intType)
    else if x `elem` words "< <= == >= > != "
        then ((intType, intType),boolType)
        else if x `elem` words "and or"
            then ((boolType, boolType), boolType)
            else error (x ++ " is not operator")


inferDefs :: Gamma -> [Def] -> InferState (M.Map String Type, [Def])
inferDefs gamma defs = do
    let names = fmap name defs
    let exprs = fmap expr defs
    ts0 <- genTVars names
    let gamma' = M.fromList ts0 : gamma
    (texprs, exprs) <- fmap unzip $ flipInOut $ map (infer gamma') exprs
    tdefs <- flipInOut $ fmap (find . snd) ts0
    flipInOut $ unify <$> tdefs <*> texprs
    tdefs <- flipInOut $ fmap find tdefs
    return (M.fromList $ zip names tdefs, map (uncurry $ uncurry Def) (zip (zip tdefs names) exprs))


doInfer :: Expr -> Either String (Type, Expr)
doInfer e = evalState (runExceptT $ infer [] e) (M.empty, 0)

doInferDefs defs = evalState (runExceptT $ inferDefs [] defs) (M.empty, 0)
