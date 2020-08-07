module CoreLang ( V(..)
                , Cons(..)
                , Expr(..)
                , module Type
                , Pat(..)) where
import Type

newtype V = V Int deriving(Eq)

instance Show V where
       show (V i) = if i < 0 then '$' : show (-i) else '%' : show i

data Cons a = Int Int
            | Bool Bool
       deriving(Show, Eq)

data Expr = Var V
          | Val (Cons Expr)
          | OP String [Expr]
          | Abs [(V, Type)] Expr 
          | TAbs [TV] Expr
          | App Expr [Expr]
          | TApp Expr [Type]
          | LetR [(V, Type, Expr)] Expr
          | Case Expr [(Pat, Expr)]
       deriving(Show, Eq)

data Pat = C (Cons Pat)
         | Wildcard 
         | String deriving (Show, Eq)