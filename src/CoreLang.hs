module CoreLang ( V(..)
                , Cons(..)
                , Expr(..)
                , module Type
                , Pat(..)) where
import Type
import Data.List (intersperse)
import Debug.Trace (trace)

type V = String 

data Cons a = Int Int
            | Bool Bool
       deriving(Eq)

instance Show (Cons a) where
       show (Int x) = show x
       show (Bool x) = show x

data Expr = Var V
          | Val (Cons Expr)
          | OP String [Expr]
          | Abs [(V, Type)] Expr 
          | TAbs [TV] Expr
          | App Expr [Expr]
          | TApp Expr [Type]
          | LetR [(V, Type, Expr)] Expr
          | Case Expr [(Pat, Expr)]
       deriving(Eq)

showOP s es = '(' : s ++ ' ' : sjoin " " (map show es) ++ ")"

sjoin s = concat . intersperse s

instance Show Expr where
       show (Var v) = v
       show (Val x) = show x
       show (OP op exprs) = showOP op exprs
       show (Abs params body) =
              let sparams = case params of 
                                   [(v, t)] -> v ++ ':': show t
                                   _ -> '(': (sjoin ", " $ map (\(v, t) -> v ++ ':' : show t) params) ++ ")"
                  sbody = show body
              in "λ" ++ sparams ++ '.' : sbody
       show (TAbs params body) = 
              let sbody = show body
                  sparams = case params of
                                   [x] -> show x
                                   xs -> '(' : (sjoin ", " $ map show params) ++ ")"
              in "Λ" ++ sparams ++ '.' : sbody
       show (App f args) = 
              let sf = if isAbsOrLetR f then '(' : show f ++ ")" else show f
                  sargs = sjoin ", " $ map show args
              in sf ++ '(':sargs ++ ")"
       show (TApp f args) = 
              let sf = if isAbsOrLetR f then '(' : show f ++ ")" else show f
                  sargs = sjoin ", " $ map show args
              in sf ++ '<':sargs ++ ">"
       show (LetR bindings body) = "let " ++ showBindings bindings ++ " in " ++ show body where
              showBindings bindings = "{" ++ sjoin ", " (map showbinding bindings) ++ "}"
              showbinding (v, t, e) = show v ++ ':' : show t ++ " = " ++ show e
       show (Case e cs) = "case " ++ show e ++ ' ' : showCS cs where
              showCS cs =  "{" ++ sjoin " | " (map showC cs) ++ "}"
              showC (p, e) = show p ++ "->" ++ show e

isAbsOrLetR (Abs _ _) = True 
isAbsOrLetR (TAbs _ _) = True 
isAbsOrLetR (LetR _ _) = True 
isAbsOrLetR _ = False

isAbsLetROrApp (Abs _ _) = True 
isAbsLetROrApp (TAbs _ _) = True 
isAbsLetROrApp (LetR _ _) = True 
isAbsLetROrApp (App _ _) = True 
isAbsLetROrApp (TApp _ _) = True 
isAbsLetROrApp _ = False

data Pat = C (Cons Pat)
         | Wildcard 
         | PV V deriving (Eq)

instance Show Pat where
       show (C x) = show x
       show Wildcard = "_"
       show (PV v) = show v