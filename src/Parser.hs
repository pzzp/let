module Parser where

import qualified Ast as A
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad (liftM, liftM2)

languageDef = emptyDef {
    T.commentLine = ";"
   ,T.identStart = letter <|> char '_'
   ,T.identLetter = alphaNum <|> char '_'
   ,T.reservedNames = ["if", "then", "else", "true", "false", "let", "def", "fun", "and", "or", "not", "in"]
}

lexer = T.makeTokenParser languageDef
ident = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
int = T.integer lexer
space = T.whiteSpace lexer
braces = T.braces lexer
commaSep = T.commaSep lexer
comma = T.comma lexer

prefixOp s = Prefix $ reservedOp s >> (return $ A.UnOp s)
infixOp s = Infix (reservedOp s >> return (A.BinOp s)) AssocLeft

operators = [
    [prefixOp "-", prefixOp "not"]
   ,[infixOp "*", infixOp "/"]
   ,[infixOp "+", infixOp "-"]
   ,[infixOp "<", infixOp ">", infixOp "<=", infixOp ">="]
   ,[infixOp "==", infixOp "!="]
   ,[infixOp "and"]
   ,[infixOp "or"]]

topExpr = buildExpressionParser operators term

term = do
    f <- atom
    args <- many $ parens $ commaSep topExpr  -- f(e1, e2)(e3,e4)(e5, e6, ...)
    return $ foldl A.App f args


atom = parens topExpr
   <|> A.Int . fromInteger <$> int
   <|> (reservedOp "true" >> return (A.Bool True))
   <|> (reservedOp "false" >> return (A.Bool False))
   <|> lamb
   <|> block
   <|> letExpr
   <|> ifExpr
   <|> liftM A.Var ident

ifExpr = do
    reserved "if"
    a <- topExpr
    b <- topExpr
    c <- topExpr
    return $ A.If a b c

letExpr = do
    reserved "let"
    let b = liftM2 (,) (ident <* reservedOp "=") topExpr
    firstBinding <- b
    restBinding <- many $ try $ comma *> b
    optional comma
    reserved "in"
    body <- topExpr
    return $ A.Let (firstBinding:restBinding) body

block = braces $ liftM2 A.Block (many def) topExpr

lamb = reserved "fun" *> liftM2 A.Lamb (commaSep ident <* reserved "=>") topExpr

def = do
    reserved "def"
    name <- ident
    params <- optionMaybe $ parens $ commaSep ident
    reservedOp "="
    body <- topExpr
    return $ A.Def name $ case params of
        Just params -> A.Lamb params body
        Nothing -> body


parserInRepl = spaces *> (liftM Left def <|> liftM Right topExpr) <* eof