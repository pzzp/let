module Check where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Lang
import Data.List (intersperse, partition)
import Debug.Trace (trace)


flipInOut :: Monad m => [m a] -> m [a]
flipInOut [] = return []
flipInOut (x:xs) = do
    x <- x
    xs <- flipInOut xs
    return $ x:xs

type Gamma = [(M.Map String Type, S.Set TV)]
type Subst =  M.Map TV Type
type InferState = ExceptT String (State (Subst, TV))


isFreeTVarOfGamma :: Gamma -> TV -> Bool
isFreeTVarOfGamma [] _ = False
isFreeTVarOfGamma ((_, fv):xs) x = 
    if x `S.member` fv then True
    else isFreeTVarOfGamma xs x


extendGamma :: [(String, Type)] -> Gamma -> Gamma
extendGamma ms g = 
    let fv = foldl getFreeTVar S.empty (map snd ms) 
        env = M.fromList ms
    in (env, fv) : g

lookupGamma :: String -> Gamma -> InferState Type
lookupGamma s g = lookupGamma' s (map fst g) where
    lookupGamma' :: String -> [M.Map String Type] -> InferState Type
    lookupGamma' s [] = throwError $ "cannot find variable " ++ s
    lookupGamma' s (d:ds) = case M.lookup s d of
        Nothing -> lookupGamma' s ds
        Just t -> return t

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

infer :: Gamma -> Expr -> InferState Type
infer _ e@(Bool _) = return boolType
infer _ e@(Int _) = return intType
infer gamma (Var name) = lookupGamma name gamma >>= inst
infer gamma (Lamb params body) = do
    checkDup params
    ms <- genTVars params
    let gamma' =  extendGamma ms gamma
    tbody <- infer gamma' body
    tparams <- flipInOut $ fmap (find . snd) ms
    let t = TFunc tparams tbody
    return t
infer gamma (App f args) = do
    tf <- infer gamma f
    targs <- flipInOut $ fmap (infer gamma) args
    tr <- TVar <$> genTVar
    unify tf (TFunc targs tr)
    tr <- find tr
    return tr
infer gamma (Block bindings expr) = do
    (types, names, _) <- unzip3 <$> inferBindings gamma bindings
    let gamma' = extendGamma (zip names types) gamma
    infer gamma' expr
infer gamma (If a b c) = do
    ta <- infer gamma a
    tb <- infer gamma b
    tc <- infer gamma c
    unify boolType ta
    unify tb tc
    return tc
infer gamma (UnOp op expr) = do
    t <- infer gamma expr
    let (te, tr) = primUnOpType op
    unify te t
    return tr
infer gamma (BinOp op e1 e2) = do
    t1 <- infer gamma e1
    t2 <- infer gamma e2
    let ((ta, tb), tr) = primBinOpType op
    unify ta t1
    unify tb t2
    return tr

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
    t2 <- infer gamma expr
    unify t1 t2
    return (t1, name, expr)

doInfer e = evalState (runExceptT $ infer [] e) (M.empty, TV 0)

doInferDef def = fmap head $ doInferDefs [def] 

doInferDefs defs = evalState (runExceptT $ inferBindings [] defs) (M.empty, TV 0)
