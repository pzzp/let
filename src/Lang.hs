module Lang ( module Type
            , Expr(..)
            , Binding
            , getBindingName
            , getBindingType ) where

import Data.List(intersperse)
import Type

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


getBindingName :: Binding -> String
getBindingName (_, name, _) = name

getBindingType :: Binding -> Type
getBindingType (t, _, _) = t

