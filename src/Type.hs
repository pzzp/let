module Type where
import Data.List (intersperse)
import qualified Data.Set as S

newtype TV = TV Int deriving (Eq, Ord)

instance Show TV where
    show (TV x) = if x == -1 then "" else if x < 26 then [toEnum (fromEnum 'a' + x)] else "t" ++ show (x - 26)

data Type = 
    TCons String
   |TFunc [Type] Type
   |TVar TV
   |Forall [TV] Type
   deriving (Eq)


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