module Cps where
import Prelude
import CoreLang
import qualified Lang as L
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Cont
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromJust)
import Debug.Trace (trace)
import Common
import Data.List (group, sort)


type Binding = ((Name, Type), Expr)

primOpType x = 
    if x `elem` words "+ - * /"
    then intType
    else if x `elem` words "< <= == >= > != and or not"
        then boolType
        else error (x ++ " is not operator")

lookupGamma' s g = fromJust $ lookupGamma s g

isTVar :: Type -> Bool
isTVar (TVar _ ) = True
isTVar _ = False

rmDups :: Eq a => [a] -> [a]
rmDups = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

getSubst :: Type -> Type -> M.Map TV Type -> M.Map TV Type
getSubst (TVar x) t sub = M.insert x t sub
getSubst (TFunc xs a) (TFunc ys b) sub = getSubstMany xs ys $ (getSubst a b sub)
getSubst (Forall _ _) _ _ = error "[higher rank type] if this happen, type reconstructing must has problem."
getSubst _ _ sub = sub

getSubstMany :: [Type] -> [Type] -> M.Map TV Type -> M.Map TV Type
getSubstMany xs ys sub = foldr (uncurry getSubst) sub (zip xs ys)

l2cl' :: Gamma -> L.Expr -> (Expr, Type)
l2cl' gamma (L.Var x) = let t = lookupGamma' x gamma in (Var x, t)
l2cl' _ (L.Int x) = (Val $ Int x, intType)
l2cl' _ (L.Bool x) = (Val $ Bool x, intType)
l2cl' gamma (L.Lamb params (body, tbody)) = 
    let gamma' = extendGamma params gamma
        (body', tbody) = l2cl' gamma' body
        paramtypes = map snd params
        abs = Abs params body'
        t = TFunc paramtypes tbody
        tvars = filter (isFreeTVarOfGamma gamma') $ S.toList $ getFreeTVar S.empty t
    in case tvars of
        [] -> (abs, t)
        xs -> (TAbs xs abs, t)
l2cl' gamma (L.App f args) = -- TODO
    let (f', tf) = l2cl' gamma f
        (args', targs) = unzip $ map (l2cl' gamma) args
    in case tf of
        TFunc tps r -> (App f' args', r)
        Forall tvars t@(TFunc tparams r) ->
            let subst = getSubstMany tparams targs M.empty
                argsOfTabs = catMaybes $ map (\k -> M.lookup k subst) tvars
                restTVars = filter (not . (`M.member` subst)) tvars
                r' = tsub subst r 
                app = case argsOfTabs of
                        [] -> App f' args'
                        _ -> App (TApp f' argsOfTabs) args'
            in trace ("app= " ++ show app) (app, r')
l2cl' gamma (L.Block bindings body) = 
    let (types, names, bexprs) = unzip3 bindings
        gamma' = extendGamma (zip names types) gamma
        (bexprs', _) = unzip $ map (l2cl' gamma') bexprs
        (body', tbody) = l2cl' gamma' body
    in (LetR (zip (zip names types) bexprs') body', tbody)
l2cl' gamma (L.If a b c) = 
    let (a', _) = l2cl' gamma a
        (b', tb) = l2cl' gamma b
        (c', _) = l2cl' gamma c
        expr = Case a' [(C $ Bool True, b'), (C $ Bool False, c')]
    in    (expr, tb)
l2cl' gamma (L.BinOp op a b) = 
    let (a', _) = l2cl' gamma a
        (b', _) = l2cl' gamma b
    in (OP op [a', b'], primOpType op)
l2cl' gamma (L.UnOp op a) = 
    let (a', _) = l2cl' gamma a
    in (OP op [a'], primOpType op)

l2cl gamma expr = fst $ l2cl' gamma expr

l2clProg :: [(Type, String, L.Expr)] -> [Binding]
l2clProg  bindings = 
    let (types, names, bexprs) = unzip3 bindings
        gamma = extendGamma (zip names types) emptyGamma
        (bexprs', _) = unzip $ map (l2cl' gamma) bexprs
    in zip (zip names types) bexprs'


tsub :: M.Map TV Type -> Type -> Type
tsub sub t@(TVar x) = case M.lookup x sub of
                        Just t -> t
                        Nothing -> t
tsub sub t@(TCons _) = t
tsub sub (TFunc tp tr) = TFunc (map (tsub sub) tp) (tsub sub tr)
tsub sub (Forall tvs t) = tsub (foldr M.delete sub tvs) t


type St = State (Int, Int)

freshKontParam :: St (String, Type)
freshKontParam = do
    (x, y) <- get
    let alpha = TV 0
    let beta = TV x
    let kname = "k#" ++ show y
    let ktype = Forall [alpha] $ TFunc [(TVar alpha)] (TVar beta)
    put ((x - 1), y + 1)
    return (kname, ktype)

freshName :: String -> St String
freshName s = do
    (x, y) <- get
    put (x, y + 1)
    return $ s ++ "#" ++ show y

apply :: [(Expr -> Expr) -> Expr] -> ([Expr] -> Expr) -> Expr
apply [] k = k []
apply (x:xs) k = x (\e -> apply xs (\ys -> k $ e:ys))

cps :: Gamma -> Expr -> St ((Expr->Expr)-> Expr, Type)
cps gamma e@(Var x) = return (\k -> k e, lookupGamma' x gamma)
cps gamma e@(Val (Int _)) = return (\k -> k e, intType)
cps gamma (Abs params body) = do
    let gamma' = extendGamma params gamma
    (bodyTrans, bodyType) <- cps gamma' body
    kParam@(kname, kType) <- freshKontParam
    let t = TFunc (kType : map snd params) bodyType
    let trans = \k -> k $ Abs (kParam:params) (bodyTrans $ \e -> (App (Var kname) [e]))
    return (trans, t)
cps gamma (OP op operands) = do
    let t = primOpType op
    (operandsTrans, _) <- unzip <$> mapM (cps gamma) operands
    let trans = \k -> apply operandsTrans (\xs -> k $ OP op xs)
    return (trans, t)
cps gamma (TAbs tparams expr) = do
    (exprTrans, t) <- cps gamma expr
    return (\k -> TAbs tparams $ exprTrans k, Forall tparams t)
cps gamma (App f args) = do
    (k, ktype) <- freshKontParam
    (ftrans, tf) <- cps gamma f
    (argsTrans, argTypes) <- unzip <$> mapM (cps gamma) args
    let (TFunc paramTypes tr) = tf
    let t = case lookup tr $ zip argTypes paramTypes of
            Just x -> x
            Nothing -> tr
    a <- freshName "a"
    let k' k f args = let kont = case (k (Var a)) of
                                    App k' [(Var _)] -> k'
                                    e -> Abs [(a, t)] e
                      in App f (kont : args)
    let trans = \k -> ftrans $ \f -> apply argsTrans $  k' k f
    return (trans, t)
cps gamma (TApp tabs targs) = do
    (tabsTrans, tabsType) <- cps gamma tabs
    let (Forall tvs tbody) = tabsType
    let t = tsub (M.fromList $ zip tvs targs) tbody
    trace ("tabs=" ++ show tabs)return (\k -> k $ TApp (tabsTrans id) targs, t)
cps gamma (LetR bindings expr) = do
    let (nameAndTypes, bexprs) = unzip bindings
    let gamma' = extendGamma nameAndTypes gamma
    let (cpsedBexprs, types') = unzip $ map (cpsExpr' gamma' id) bexprs
    let bindings' = zip (zip (fst <$> nameAndTypes) types') cpsedBexprs
    (exprTrans, exprType) <- cps gamma' expr
    return (\k -> LetR bindings' (exprTrans k), exprType)
cps gamma (Case e cases) = do
    (eTrans, eType) <- cps gamma e
    let (pats, branches) = unzip cases
    (branchesTrans, ts) <- unzip <$> mapM (cps gamma) branches
    let t = head ts
    let cpsedCases k = zip pats $ map ($k) branchesTrans
    return (\k -> eTrans $ \x -> Case x $ cpsedCases k, t)

cpsExpr' gamma kont expr = let (trans, t) = evalState (cps gamma expr) (-2, 0) in (trans kont, t)

cpsExpr :: Gamma -> (Expr -> Expr) -> Expr -> Expr
cpsExpr gamma kont expr = fst $ cpsExpr' gamma kont expr

cpsTopExpr :: (Expr -> Expr) -> Expr -> Expr
cpsTopExpr = cpsExpr emptyGamma

cpsTopExprEmpytCtx :: Expr -> Expr
cpsTopExprEmpytCtx = cpsTopExpr id

cpsTopExprDynCtx expr = flip evalState (-2, 0) $ do
    kParam@(kname, _) <- freshKontParam
    let k x = App (Var kname) [x]
    let cpsed = cpsTopExpr k expr
    return $ Abs [kParam] cpsed


cpsProgram :: [Binding] -> [Binding]
cpsProgram bindings = 
    let (nameAndTypes, bexprs) = unzip bindings
        gamma = extendGamma nameAndTypes emptyGamma
        cpsedBexprs = map (cpsExpr gamma id) bexprs
        bindings' = zip nameAndTypes cpsedBexprs
    in bindings'

