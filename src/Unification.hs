module Unification where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Lang
import Debug.Trace (trace)
import Data.List (intersperse, partition)


flipInOut :: Monad m => [m a] -> m [a]
flipInOut [] = return []
flipInOut (x:xs) = do
    x <- x
    xs <- flipInOut xs
    return $ x:xs

type Gamma = ([M.Map String Type], S.Set Int)
type Subst =  M.Map Int Type
type InferState = ExceptT String (State (Subst, Int))

emptyGamma :: ([M.Map k a1], S.Set a2)
emptyGamma = ([M.empty], S.empty)

tFreeVarInGamma :: Gamma -> S.Set Int
tFreeVarInGamma gamma = snd gamma

tFreeVar :: S.Set Int -> Type -> S.Set Int
tFreeVar = tFreeVar' S.empty 

tFreeVar' :: S.Set Int -> S.Set Int -> Type -> S.Set Int
tFreeVar' boundTVars freeTVars (TVar x) = if x `S.member` boundTVars then freeTVars else S.insert x freeTVars
tFreeVar' boundTVars freeTVars (TFunc tparams tbody) = foldl (tFreeVar' boundTVars) freeTVars (tbody:tparams)
tFreeVar' boundTVars freeTVars (Forall bs t) = tFreeVar' (foldr S.insert boundTVars bs) freeTVars t
tFreeVar' _ freeTVars _ = freeTVars

extendGamma :: [(String, Type)] -> Gamma -> Gamma
extendGamma ms (env, fv) = (M.fromList ms : env, foldl tFreeVar fv (map snd ms)) 

lookupGamma :: String -> Gamma -> InferState Type
lookupGamma s g = lookupGamma' s (fst g) where
    lookupGamma' :: String -> [M.Map String Type] -> InferState Type
    lookupGamma' s [] = throwError $ "cannot find variable " ++ s
    lookupGamma' s (d:ds) = case M.lookup s d of
        Nothing -> lookupGamma' s ds
        Just t -> return t

union :: Int -> Type -> InferState ()
union k t = if TVar k == t then return () else do
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

generalize :: Gamma -> Type -> Type 
generalize gamma t = 
    case S.toList $ tFreeVar' (tFreeVarInGamma gamma) S.empty t of
        [] -> t
        bs -> Forall bs t

unify :: Type -> Type -> InferState ()
unify a b = do
    a <- inst a >>= find
    b <- inst b >>= find
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
infer gamma e@(Var _ name) = do
    t <- lookupGamma name gamma >>= inst
    return (t, e)
infer gamma (Lamb params body) = do
    checkDup params
    ts <- genTVars params
    let gamma' =  extendGamma ts gamma
    (tbody, body) <- infer gamma' body
    tparams <- flipInOut $ fmap (find . snd) ts
    return (TFunc tparams tbody, Lamb params body)
infer gamma (App f args) = do
    (tf, f) <- infer gamma f
    (targs, args) <- unzip <$> (flipInOut $ fmap (infer gamma) args)
    tr <- TVar <$> genTVar
    unify tf (TFunc targs tr)
    tr <- find tr
    return (tr, App f args)
infer gamma (Block defs expr) = do
    (tbody, bindings, body) <- inferBlockExpr gamma defs expr
    return (tbody, Block bindings body)
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

isTVar (TVar _) = True
isTVar _ = False

inferBindings :: Gamma -> [Binding] -> InferState [Binding]
inferBindings gamma bindings = do
    let names = fmap getBindingName bindings
    checkDup names
    ms <- genTVars names
    let gamma' = extendGamma ms gamma
    bindings <- flipInOut $ fmap (inferBinding gamma') bindings
    types <- flipInOut $ fmap (find . getBindingType) bindings
    let bindings' = map (\((_, name, body), t) -> (t, name, body)) (zip bindings types)
    let infvalues = map (\(_, name, _) -> name) $ filter (\(t, _, _) -> isTVar t) bindings'
    if infvalues /= [] then
        throwError $ "Cannot create infinite value: " ++ (concat $ intersperse ", " infvalues)
    else
        trace ("trace1 " ++ show infvalues) return bindings'

inferBinding :: Gamma -> Binding -> InferState Binding
inferBinding gamma (_, name, expr) = do
    t1 <- lookupGamma name gamma
    (t2, expr) <- infer gamma expr
    unify t1 t2
    t <- find t2
    let t' = generalize gamma t
    return (t', name, expr)

inferBlockExpr :: Gamma -> [Binding] -> Expr -> InferState (Type, [Binding], Expr)
inferBlockExpr gamma bindings body = do
    bindings <- inferBindings gamma bindings
    let (types, names, _) = unzip3 bindings
    let gamma' = extendGamma (zip names types) gamma
    (tbody, body) <- infer gamma' body
    return (tbody, bindings, body)


doInfer :: Expr -> Either String (Type, Expr)
doInfer e = evalState (runExceptT $ infer emptyGamma e) (M.empty, 0)

doInferDef def = fmap head $ doInferDefs [def] 

doInferDefs defs = evalState (runExceptT $ inferBindings emptyGamma defs) (M.empty, 0)
