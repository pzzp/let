module Parser where

import qualified Ast as A
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme = L.lexeme sc

symbol = L.symbol sc

inparen = between (symbol "(")  (symbol ")")

ident = lexeme $ (:) <$> (char1 <|> letterChar) <*> (many $ char1 <|> alphaNumChar) where
    char1 = oneOf "+-*/<=>&|~.#:"

int = A.Int . fromIntegral <$> L.signed space1 (lexeme L.decimal)

bool = A.Bool <$> ((\_ -> True) <$> symbol "true" <|> (\_ -> False) <$> symbol "false")

var = A.Var <$> ident

lamb = inparen $ do
    symbol "lambda" <|> symbol "λ"
    xs <- inparen $ many ident
    body <- exprParser
    return $ A.Lamb xs body
app = inparen $ do
    f <- exprParser
    args <- many exprParser
    return $ A.App f args

letExpr = inparen $ do
    symbol "let"
    let binding = do
        name <- ident
        v <- exprParser
        return (name, v)
    bindings <- many $ inparen binding
    body <- exprParser
    return $ A.Let bindings body

block = inparen $ do
    defs <- many (def1 <|> def2)
    r <- exprParser
    return $ A.Block defs r

ifExpr = inparen $ do
    symbol "if"
    a <- exprParser
    b <- exprParser
    c <- exprParser
    return $ A.If a b c

exprParser =  bool <|> int <|> var <|> lamb <|> app <|> letExpr <|> block <|> ifExpr

def1 = inparen $ do
    name <- ident
    e <- exprParser
    return $ A.Def name e

def2 = inparen $ do
    (name, params) <-inparen $ (,) <$> ident <*> many ident
    body <- exprParser
    return $ A.Def name $ A.Lamb params body


parseExpr1 = parse exprParser "test" 
parseDef1 = parse (def1 <|> def2) "test" 

parse1 = parse

integer       = lexeme L.decimal
signedInteger = L.signed (L.space empty empty empty) integer