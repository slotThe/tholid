module Parser (read) where

import Prelude hiding (read)
import Types
import Util

import Data.Text                  qualified as T
import Text.Megaparsec            qualified as P
import Text.Megaparsec.Char       qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Char (isSpace)
import Text.Megaparsec ((<?>))


type Parser = P.Parsec Void Text

read :: Text -> Either String [Expr]
read inp = case P.parse (space *> pExpr `P.sepBy` space) "" inp of
  Left err    -> Left $ P.errorBundlePretty err
  Right exprs -> Right exprs

inList :: Parser p -> Parser p
inList p = symbol "(" *> p <* symbol ")"

pExpr :: Parser Expr
pExpr = P.try pNil <|> pSymbol <|> pInt <|> pBool <|> pList <|> pUnquote

pNil :: Parser Expr
pNil = takeSymbol >>= \s -> guard (s == "nil") $> ENil <?> "nil"

pInt :: Parser Expr
pInt = EInt <$> L.signed space L.decimal <?> "integer"

pBool :: Parser Expr
pBool = EBool <$> (symbol "#t" $> True <|> symbol "#f" $> False) <?> "boolean"

pUnquote :: Parser Expr
pUnquote = fmap EList $ (\a b -> [a, b]) <$> (symbol "," $> ESymbol ",")
                                         <*> pExpr

pSymbol :: Parser Expr
pSymbol = ESymbol <$> lexeme (T.cons <$> start <*> takeSymbol) <?> "symbol"
 where
  start :: Parser Char
  start = P.letterChar <|> P.satisfy (`elem` ("+-_^*/<=>" :: String))

pList :: Parser Expr
pList = pQuote "'" "quote"        "list"
    <|> pQuote "`" "syntax-quote" "backquoted list"
 where
  pQuote :: P.Tokens Text -> Text -> String -> Parser Expr
  pQuote sym tok help = EList <$> P.choice
    [ (:) <$> (symbol sym $> ESymbol tok) <*> list  -- `(syntax-quote)
    , list
    ] <?> help
   where
    list :: Parser [Expr]
    list = inList (pExpr `P.sepBy` space)

takeSymbol :: Parser (P.Tokens Text)
takeSymbol = P.takeWhileP Nothing $ \a -> not (isSpace a) && a /= '(' && a /= ')'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: P.Tokens Text -> Parser Text
symbol = L.symbol space

space :: Parser ()
space = L.space P.space1
                (L.skipLineComment ";")
                empty                    -- no block comments
