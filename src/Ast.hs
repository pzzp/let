module Ast where

data Expr = 
    Int Int
   |Bool Bool
   |Var String
   |Lamb [String] Expr
   |App Expr [Expr]
   |Let [(String, Expr)] Expr
   |Block [Def] Expr
   |If Expr Expr Expr
   |UnOp String Expr
   |BinOp String Expr Expr
   deriving (Eq, Show)


data Def = Def {name::String, expr::Expr } deriving(Eq, Show)