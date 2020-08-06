module Lang where

import Data.List(intersperse)

data Expr = 
    Int Int
   |Bool Bool
   |Var Type String
   |Lamb Type [String] Expr
   |App Expr [Expr]
   |Block [Binding] Expr
   |If Expr Expr Expr
   |UnOp String Expr
   |BinOp String Expr Expr
   deriving (Eq, Show)


type Binding = (Type, String, Expr)

data Type = 
    TCons String
   |TFunc [Type] Type
   |TVar Int
   |Forall [Int] Type
   deriving (Eq)


getBindingName :: Binding -> String
getBindingName (_, name, _) = name

getBindingType :: Binding -> Type
getBindingType (t, _, _) = t

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