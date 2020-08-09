{-# LANGUAGE Rank2Types #-}

module Cps where
import Prelude
import CoreLang
import qualified Lang as L
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Common
import Data.List (group, sort)


data Kont = DynK V | StaK (forall a b. a->b)

getParamsTypeOfFuncType :: Type ->[Type]
getParamsTypeOfFuncType (TFunc tp _) = tp
getParamsTypeOfFuncType (Forall _ (TFunc tp _)) = tp


getBoundTVars (Forall xs _) = xs
getBoundTVars _ = []

primBinOpType x = 
    if x `elem` words "+ - * /"
    then intType
    else if x `elem` words "< <= == >= > != "
        then boolType
        else if x `elem` words "and or"
            then boolType
            else error (x ++ " is not operator")

lookupGamma' s g = fromJust $ lookupGamma s g

isTVar :: Type -> Bool
isTVar (TVar _ ) = True
isTVar _ = False

rmDups x = rmDups' x [] where
    rmDups' [] _ = []
    rmDups' (x:xs) list2
        | (x `elem` list2) = rmDups' xs list2
        | otherwise = x : rmDups' xs (x:list2)

l2cl' :: Gamma -> L.Expr -> (Expr, Type)
l2cl' gamma (L.Var x) = let t = lookupGamma' x gamma in (Var x, t)
l2cl' _ (L.Int x) = (Val $ Int x, intType)
l2cl' _ (L.Bool x) = (Val $ Bool x, intType)
l2cl' gamma (L.Lamb params (body, tbody)) = 
    let gamma' = extendGamma params gamma
        (body', tbody) = l2cl' gamma' body
        paramtypes = map snd params
        f [] = []
        f ((TVar x):xs) = if isFreeTVarOfGamma gamma' x then x:f xs else f xs
        f (_:xs) = f xs
        abs = Abs params body'
        t = TFunc paramtypes tbody
    in case f paramtypes of
        [] -> (abs, t)
        xs -> (TAbs (rmDups xs) abs, t)
l2cl' gamma (L.App f args) = 
    let (f', tf) = l2cl' gamma f
        (args', targs) = unzip $ map (l2cl' gamma) args
        (tparams, tr) = case tf of
                    Forall _ (TFunc tparams _) -> (tparams, tr)
                    TFunc tparams _ -> (tparams, tr)
        subst = zip tparams targs
        t = case lookup tr subst of
                        Just t -> t
                        Nothing -> tr
        tabsArgs = rmDups $ map snd $ filter (\(p, _) -> isTVar p ) subst
    in case tabsArgs of
        [] -> (App f' args', t)
        _ -> (App (TApp f' tabsArgs) args', t)
        
l2cl' gamma (L.Block bindings body) = 
    let (types, names, bexprs) = unzip3 bindings
        gamma' = extendGamma (zip names types) gamma
        (bexprs', ts') = unzip $ map (l2cl' gamma') bexprs
        (body', tbody) = l2cl' gamma' body
    in (LetR (zip3 names types bexprs') body', tbody)
l2cl' gamma (L.If a b c) = 
    let (a', _) = l2cl' gamma a
        (b', tb) = l2cl' gamma b
        (c', _) = l2cl' gamma c
        expr = Case a' [(C $ Bool True, b'), (C $ Bool False, c')]
    in    (expr, tb)
l2cl' gamma (L.BinOp op a b) = 
    let (a', _) = l2cl' gamma a
        (b', _) = l2cl' gamma b
    in (OP op [a', b'], primBinOpType op)
l2cl' gamma (L.UnOp op a) = 
    let (a', _) = l2cl' gamma a
    in (OP op [a'], primBinOpType op)

l2cl gamma expr = fst $ l2cl' gamma expr

l2clProg :: [(Type, String, L.Expr)] -> [(String, Type, Expr)]
l2clProg  bindings = 
    let (types, names, bexprs) = unzip3 bindings
        gamma = extendGamma (zip names types) emptyGamma
        (bexprs', types') = unzip $ map (l2cl' gamma) bexprs
    in zip3 names types bexprs'



-- app :: Kont -> Expr -> Expr
-- app (DynK k) expr = App

-- cps :: S.Set String -> Kont -> Expr -> Expr