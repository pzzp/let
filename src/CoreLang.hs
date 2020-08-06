module CoreLang ( ID
                , Cons(..)
                , Expr(..)
                , module Type
                , Pat) where
import Type

type ID = Int

data Cons a = Int Int
            | Bool Bool
       deriving(Show, Eq)

data Expr = Var String
          | Val (Cons Expr)
          | OP String [Expr]
          | Abs [(String, Type)] Expr 
          | TAbs [ID] Expr
          | App Expr [Expr]
          | TApp Expr [Type]
          | LetR [(String, Type, Expr)] Expr
          | Case [(Pat, Expr)]
       deriving(Show, Eq)

data Pat = C (Cons Pat)
         | Wildcard 
         | String deriving (Show, Eq)