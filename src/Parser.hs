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

-- | Try to parse a bunch of lisp expression from the given string.
read :: Text -> Either String [Expr]
read inp = case P.parse (space *> pExpr `P.sepBy` space) "" inp of
  Left err    -> Left $ P.errorBundlePretty err
  Right exprs -> Right exprs

-- | Parse a lisp expression.
pExpr :: Parser Expr
pExpr = P.try pNil <|> pSymbol <|> pInt <|> pBool <|> pList <|> pUnquote

-- | Parse @nil@.
pNil :: Parser Expr
pNil = takeSymbol >>= \s -> guard (s == "nil") $> ENil <?> "nil"

-- | Parse an integer.
pInt :: Parser Expr
pInt = EInt <$> L.signed space L.decimal <?> "integer"

-- | Parse a boolean value; either @#t@ or @#f@.
pBool :: Parser Expr
pBool = EBool <$> (symbol "#t" $> True <|> symbol "#f" $> False) <?> "boolean"

-- | Parse an arbitrary symbol.
pSymbol :: Parser Expr
pSymbol = ESymbol <$> lexeme (T.cons <$> start <*> takeSymbol) <?> "symbol"
 where
  start :: Parser Char
  start = P.letterChar <|> P.satisfy (`elem` ("+-_^*/<=>" :: String))

-- | Parse a quoted list, either by a normal quote or by a backquote.
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

-- | Parse an unquote or a splice.
pUnquote :: Parser Expr
pUnquote = fmap EList $
  (\a b -> [a, b]) <$> (   symbol ",@" $> ESymbol ",@"
                       <|> symbol ","  $> ESymbol ","
                       )
                   <*> pExpr

inList :: Parser p -> Parser p
inList p = symbol "(" *> p <* symbol ")"

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
