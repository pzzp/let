module Type where
import Data.List (intersperse)
import qualified Data.Set as S


data Type = 
    TCons String
   |TFunc [Type] Type
   |TVar Int
   |Forall [Int] Type
   deriving (Eq)

showTVar x = if x == -1 then "" else if x < 26 then [toEnum (fromEnum 'a' + x)] else "t" ++ show (x - 26)

instance Show Type where
    show (TCons t) = t
    show (TFunc xs r) = "(" ++ (concat $ intersperse ", " $ map show xs) ++ ")" ++ "->" ++ show r
    show (TVar x) = showTVar x
    show (Forall xs t) = case xs of
        [] -> show t
        [x] -> "∀ " ++ showTVar x ++ ": " ++ show t
        _ -> "∀(" ++ concat (intersperse ", " (map showTVar xs)) ++ "): " ++ show t

boolType = TCons "Bool"

intType = TCons "Int"

toBeTyped = TVar (-1)


getTFreeVar :: S.Set Int -> Type -> S.Set Int
getTFreeVar = getTFreeVar' S.empty 

getTFreeVar' :: S.Set Int -> S.Set Int -> Type -> S.Set Int
getTFreeVar' boundTVars freeTVars (TVar x) = if x `S.member` boundTVars then freeTVars else S.insert x freeTVars
getTFreeVar' boundTVars freeTVars (TFunc tparams tbody) = foldl (getTFreeVar' boundTVars) freeTVars (tbody:tparams)
getTFreeVar' boundTVars freeTVars (Forall bs t) = getTFreeVar' (foldr S.insert boundTVars bs) freeTVars t
getTFreeVar' _ freeTVars _ = freeTVars