-- {-# LANGUAGE FlexibleContexts #-}
module TypeInference where
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(intersperse)
import Ast
import Data.Maybe(isJust)

flipInOut :: Monad m => [m a] -> m [a]
flipInOut [] = return []
flipInOut (x:xs) = do
    x <- x
    xs <- flipInOut xs
    return $ x:xs

data Type = 
    TCons String
   |TFunc [Type] Type
   |TVar Int
   |Forall [Int] Type
   deriving (Eq)
instance Show Type where
    show (TCons t) = t
    show (TFunc xs r) = "(" ++ (concat $ intersperse ", " $ map show xs) ++ ")" ++ "->" ++ show r
    show (TVar x) = if x < 26 then show $ (toEnum (fromEnum 'a' + x) :: Char) else "t" ++ show (x - 26)
    show (Forall xs t) = "∀" ++ (show $ map (show . TVar) xs) ++ show t


boolType = TCons "Bool"
intType = TCons "Int"

type Gamma =  [M.Map String Type]
type Subst =  M.Map Int Type



type InferState = ExceptT String (State (Subst, Int))

insertToSubst :: Int -> Type -> InferState ()
insertToSubst k t = do
    (subst, g) <- get
    let subst = M.insert k t subst
    put (subst, g)

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
unify (TVar a) y = insertToSubst a y
unify x (TVar b) = insertToSubst b x
unify (TFunc ps1 r1) (TFunc ps2 r2) = unifyMany ps1 ps2 >> unify r1 r2 where
    unifyMany [] [] = return ()
    unifyMany (x: xs) (y: ys) = unify x y >> unifyMany xs ys
    unifyMany _ _ = throwError "Arity not match"
unify (TCons a) (TCons b) = if a == b then return () else throwError $ "type mismatch " ++ show a ++ " " ++ show b
unify _ _ = throwError "type error"

find :: Type -> InferState Type
find = find' S.empty where
    find' :: S.Set Int -> Type -> InferState Type
    find' s t@(TVar v) = if v `S.member` s then throwError "cannot create infinite type" else do
        subst <- fst <$> get
        let s = S.insert v s
        case M.lookup v subst of
            Nothing -> return t
            Just t -> do
                t <- find' s t
                insertToSubst v t
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

infer :: Gamma -> Expr -> InferState Type
infer _ (Bool _) = return  boolType
infer _ (Int _) = return intType
infer gamma (Var name) = lookupGamma name gamma >>= inst
infer gamma (Lamb xs body) = r where 
    getBoundTVars s b [] = b
    getBoundTVars s b ((TVar x):xs) = getBoundTVars (S.insert x s) (if x `S.member` s then b else x:b) xs
    getBoundTVars s b (x:xs) = getBoundTVars s b xs
    r = do
        m <- genTVars xs
        tbody <- infer (M.fromList m : gamma) body
        tparams <- flipInOut $ fmap (find . snd) m
        let boundTVars = getBoundTVars S.empty [] (tbody: tparams)
        let tf = TFunc tparams tbody
        return $ Forall boundTVars $ TFunc tparams tbody
infer gamma (App f args) = do
    tf <- infer gamma f
    targs <- flipInOut $ fmap (infer gamma) args
    tr <- TVar <$> genTVar
    unify tf (TFunc targs tr)
    find tr
infer gamma (Let bindings body) = do
    ts <- flipInOut $ fmap (infer gamma . snd) bindings
    let m = M.fromList $ zip (fmap fst bindings) ts
    infer (m:gamma) body
infer gamma (Block defs expr) = do
    m <- inferDefs gamma defs
    infer (m:gamma) expr
infer gamma (If a b c) = do
    ta <- infer gamma a
    tb <- infer gamma a
    tc <- infer gamma b
    unify ta boolType
    unify tb tc
    return tc

inferDefs :: Gamma -> [Def] -> InferState (M.Map String Type)
inferDefs gamma defs = do
    let names = fmap name defs
    ts0 <- genTVars names
    let m = M.fromList ts0
    ts1 <- flipInOut $ fmap (infer (m:gamma) . expr) defs
    return $ M.fromList $ zip names ts1


inferType :: Expr -> Either String Type
inferType e = evalState (runExceptT $ infer [] e) (M.empty, 0)

t = inferType