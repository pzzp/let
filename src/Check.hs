module Check where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Lang
import qualified CoreLang as CL
import Data.List (intersperse, partition)
import Debug.Trace (trace)
import Common hiding (lookupGamma)


type Subst =  M.Map TV Type
type InferState = ExceptT String (State (Subst, TV))


lookupGamma :: String -> Gamma -> InferState Type
lookupGamma s (env, _) = 
    case M.lookup s env of
        Just t -> return t
        Nothing -> throwError $ "cannot find variable " ++ s

union :: TV -> Type -> InferState ()
union k t = if TVar k == t then return () else do
    (subst, g) <- get
    let subst' = M.insert k t subst
    put (subst', g)

genTVar :: InferState TV
genTVar = do
    (s, TV tv) <- get
    put (s, TV $ tv + 1)
    return $ TV tv

inst :: Type -> InferState Type
inst = inst' M.empty where
    inst' m t@(TVar x) = 
        return $ case M.lookup x m of
            Just t ->  TVar t
            Nothing -> t
    inst' m (TFunc p r) = do
        p <- flipInOut $ fmap (inst' m) p
        r <- inst' m r
        return $ TFunc p r
    inst' m (Forall bs b) = do 
        let (bs', monads) = unzip $ (zip bs $ repeat genTVar)
        newtv <- flipInOut monads
        let ins (a, b) =  M.insert a b
        let m' = foldr ins m $ zip bs' newtv
        inst' m' b
    inst' _ t = return t

generalize :: Gamma -> Type -> Type 
generalize gamma t = 
    case filter (not . isFreeTVarOfGamma gamma) $ S.toList $ getFreeTVar S.empty t of
        [] -> t
        bs -> Forall bs t

unify :: Type -> Type -> InferState ()
unify a b = do
    a <- find a
    b <- find b
    unify' a b where
        unify' (TVar a) y = union a y
        unify' x (TVar b) = union b x
        unify' (TFunc ps1 r1) (TFunc ps2 r2) = unifyMany ps1 ps2 >> unify r1 r2 where
            unifyMany [] [] = return ()
            unifyMany (x: xs) (y: ys) = unify x y >> unifyMany xs ys
            unifyMany _ _ = throwError "Arity not match"
        unify' (TCons a) (TCons b) = if a == b then return () else throwError $ "type mismatch " ++ show a ++ " " ++ show b
        unify' a b = throwError "type error"

find :: Type -> InferState Type
find = find' S.empty where
    find' :: S.Set TV -> Type -> InferState Type
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


genTVars :: [String] -> InferState [(String, Type)]
genTVars [] = return []
genTVars (x:xs) = do
    t <- TVar <$> genTVar
    m <- genTVars xs
    return $ (x, t):m

checkDup :: [String] -> InferState ()
checkDup = checkDup' S.empty where
    checkDup' :: S.Set String -> [String] -> InferState ()
    checkDup' s [] = return ()
    checkDup' s (x: xs) = 
        if x `S.member` s 
        then throwError $ "Duplicated definition of " ++ x
        else checkDup' (S.insert x s) xs

infer :: Gamma -> Expr -> InferState (Type, Expr)
infer _ e@(Bool _) = return (boolType, e)
infer _ e@(Int _) = return (intType, e)
infer gamma e@(Var name) = do
    t <- lookupGamma name gamma >>= inst
    return (t, e)
infer gamma (Lamb params (body, _)) = do
    let paramNames =map fst params 
    checkDup paramNames
    ms <- genTVars paramNames
    let gamma' =  extendGamma ms gamma
    (tbody, body) <- infer gamma' body
    tparams <- flipInOut $ fmap (find . snd) ms
    let t = TFunc tparams tbody
    return (t, Lamb (zip paramNames tparams) (body, tbody))
infer gamma (App f args) = do
    (tf, f) <- infer gamma f
    (targs, args) <- fmap unzip $ flipInOut $ fmap (infer gamma) args
    tr <- TVar <$> genTVar
    unify tf (TFunc targs tr)
    tr <- find tr
    return (tr, App f args)
infer gamma (Block bindings rexpr) = do
    (types, names, bexprs) <- unzip3 <$> inferBindings gamma bindings
    let gamma' = extendGamma (zip names types) gamma
    (trexpr, rexpr) <- infer gamma' rexpr
    return (trexpr, Block (zip3 types names bexprs) rexpr)
infer gamma (If a b c) = do
    (ta, a) <- infer gamma a
    (tb, b) <- infer gamma b
    (tc, c) <- infer gamma c
    unify boolType ta
    unify tb tc
    return (tc, If a b c)
infer gamma (UnOp op expr) = do
    (t, expr) <- infer gamma expr
    let (te, tr) = primUnOpType op
    unify te t
    return (tr, UnOp op expr)
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


inferBindings :: Gamma -> [Binding] -> InferState [Binding]
inferBindings gamma bindings = do
    let names = fmap getBindingName bindings
    checkDup names
    ms <- genTVars names
    let gamma' = extendGamma ms gamma
    (types, names, exprs) <- fmap unzip3 $ flipInOut $ fmap (inferBinding gamma') bindings
    types <- flipInOut $ fmap find types
    let types' = map (generalize gamma) types
    let isInfValue = \ x -> case x of
                                Forall _ (TVar _) -> True
                                otherwise -> False
    let infValues = map snd $ filter (isInfValue . fst) $ zip types' names
    if infValues /= [] then
        throwError $ "Cannot create infinite value: " ++ (concat $ intersperse ", " infValues)
    else
        return $ zip3 types' names exprs

inferBinding :: Gamma -> Binding -> InferState Binding
inferBinding gamma (_, name, expr) = do
    t1 <- lookupGamma name gamma
    (t2, expr) <- infer gamma expr
    unify t1 t2
    return (t1, name, expr)

f :: Gamma -> Expr -> InferState (Type, Expr)
f _ e@(Bool _) = return (boolType, e)
f _ e@(Int _) = return (intType, e)
f gamma v@(Var x) = do
    t <- lookupGamma x gamma
    t <- find t >>= inst
    return (t, v)
f gamma (Lamb params (body, rtype)) = do
    let (paramNames, tparams) = unzip params
    tparams <- flipInOut $ map find tparams
    let params' = zip paramNames tparams
    let gamma' = extendGamma params' gamma
    (tbody, body) <- f gamma' body
    let t = TFunc tparams tbody
    return (t, Lamb params' (body, tbody))
f gamma (App fun args) = do
    (tfun, fun) <- f gamma fun
    (targs, args) <- fmap unzip $ flipInOut $ map (f gamma) args
    let TFunc tparams tr = tfun
    let t = case lookup tr (zip tparams targs) of
                Nothing -> tr
                Just x -> x
    return (t, App fun args)
f gamma (Block bindings rexpr) = do
    bindings <- f_bindings gamma bindings
    let (types, names, bexprs) = unzip3 bindings
    let gamma' = extendGamma (zip names types) gamma
    (trexpr, rexpr)  <- f gamma' rexpr
    return (trexpr, Block bindings rexpr)
f gamma (If a b c) = do
    (ta, a) <- f gamma a
    (tb, b) <- f gamma b
    (tc, c) <- f gamma c
    return (tc, If a b c)
f gamma (UnOp op expr) = do
    (t, expr) <- f gamma expr
    let (te, tr) = primUnOpType op
    return (tr, UnOp op expr)
f gamma (BinOp op e1 e2) = do
    (t1, e1) <- f gamma e1
    (t2, e2) <- f gamma e2
    let ((ta, tb), tr) = primBinOpType op
    return (tr, BinOp op e1 e2)

f_bindings gamma bindings = do
    let (types, names, bexprs) = unzip3 bindings
    types <- flipInOut $ map find types
    let gamma' = extendGamma (zip names types) gamma
    (tbexprs, bexprs) <- fmap unzip $ flipInOut $ map (f gamma') bexprs
    return $ zip3 types names bexprs


doInfer e = 
    case runState (runExceptT $ infer (M.empty, S.empty) e) (M.empty, TV 0) of
        (Left err, _) -> Left err
        (Right (t, expr), (subst, _)) -> evalState (runExceptT $ f (M.empty, S.empty) expr) (subst, TV 0)


doInferDefs defs = 
    case runState (runExceptT $ inferBindings (M.empty, S.empty) defs) (M.empty, TV 0) of
        (Left err, _) -> Left err
        (Right bindings, (subst, _)) -> evalState (runExceptT $ f_bindings (M.empty, S.empty) bindings) (subst, TV 0)


