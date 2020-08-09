module Common where
import qualified Data.Map as M
import qualified Data.Set as S
import Type





flipInOut :: Monad m => [m a] -> m [a]
flipInOut [] = return []
flipInOut (x:xs) = do
    x <- x
    xs <- flipInOut xs
    return $ x:xs

type Gamma = (M.Map String Type, S.Set TV)

emptyGamma :: Gamma
emptyGamma = (M.empty, S.empty)

isFreeTVarOfGamma :: Gamma -> TV -> Bool
isFreeTVarOfGamma (_, fv) x = x `S.member` fv

extendGamma :: [(String, Type)] -> Gamma -> Gamma
extendGamma ms (env, fv) = 
    let fv' = foldl getFreeTVar fv (map snd ms) 
        env' = foldr (\(a, b) -> M.insert a b) env ms
    in (env', fv')

lookupGamma :: String -> Gamma -> Maybe Type
lookupGamma s (env, _) = M.lookup s env