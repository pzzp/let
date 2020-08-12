module Type where
import Data.List (sort, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)


newtype TV = TV Int deriving (Eq, Ord)

instance Show TV where
    show (TV x) = 
        if x < -1 then 'k' : show (-x - 2) -- (-∞, -2], used by contiuation
        else if x == -1 then ""
        else if x < 26 then [toEnum (fromEnum 'A' + x)] 
        else "T" ++ show (x - 26)

data Type = 
    TCons String
   |TFunc [Type] Type
   |TVar TV
   |Forall [TV] Type

instance Eq Type where
    (==) (TCons a) (TCons b) = a == b
    (==) (TFunc ps1 r1) (TFunc ps2 r2) = all (uncurry $ (==)) (zip ps1 ps2) && r1 == r2
    (==) (TVar v1) (TVar v2) = v1 == v2
    (==) a@(Forall _ _) b@(Forall _ _) = 
        let (Forall bs1 t1) = compact a 
            (Forall bs2 t2) = compact b
        in bs1 == bs2 && t1 == t2
    (==) _ _ = False

compact :: Type -> Type
compact = compact' M.empty 0 where
    compact' _ _ t@(TCons _) = t
    compact' m n (TFunc ps r) = TFunc (map (compact' m n) ps) (compact r)
    compact' m _ (TVar v) = TVar $ fromMaybe v $ M.lookup v m
    compact' m n (Forall bs t) = 
        let subst = zip (sort bs) (map TV [n..])
            bs' = map snd subst
            m' = foldr (\(a, r) -> M.insert a r) m subst
        in Forall bs' $ compact' m' (n + length subst) t


instance Show Type where
    show (TCons t) = t
    show (TFunc xs r) = "(" ++ (concat $ intersperse ", " $ map show xs) ++ ")" ++ "->" ++ show r
    show (TVar x) = show x
    show (Forall xs t) = case xs of
        [] -> show t
        [x] -> "∀ " ++ show x ++ ": " ++ show t
        _ -> "∀(" ++ concat (intersperse ", " (map show xs)) ++ "): " ++ show t

boolType = TCons "Bool"

intType = TCons "Int"

toBeTyped = TVar $ TV (-1)


getFreeTVar :: S.Set TV -> Type -> S.Set TV
getFreeTVar = getFreeTVar' S.empty 

getFreeTVar' :: S.Set TV -> S.Set TV -> Type -> S.Set TV
getFreeTVar' boundTVars freeTVars (TVar x) = if x `S.member` boundTVars then freeTVars else S.insert x freeTVars
getFreeTVar' boundTVars freeTVars (TFunc tparams tbody) = foldl (getFreeTVar' boundTVars) freeTVars (tbody:tparams)
getFreeTVar' boundTVars freeTVars (Forall bs t) = getFreeTVar' (foldr S.insert boundTVars bs) freeTVars t
getFreeTVar' _ freeTVars _ = freeTVars