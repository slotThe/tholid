module Parser (read) where

import Prelude hiding (read)
import Types
import Util

import qualified Data.Text                  as T
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isSpace)


type Parser = P.Parsec Void Text

read :: Text -> Either String [Expr]
read inp = case P.parse (pExpr `P.sepBy` space) "" inp of
  Left P.ParseErrorBundle{ P.bundlePosState = ps } ->
    let line = P.sourceLine   $ P.pstateSourcePos ps
        col  = P.sourceColumn $ P.pstateSourcePos ps
        expr = P.pstateInput ps
     in Left $ "Parse error on ("
            <> show line <> ", " <> show col
            <> ") in expression " <> T.unpack expr
  Right exprs -> Right exprs

inList :: Parser p -> Parser p
inList p = symbol "(" *> p <* symbol ")"

pExpr :: Parser Expr
pExpr = P.try pNil <|> pSymbol <|> pInt <|> pBool <|> pList

pNil :: Parser Expr
pNil = takeSymbol >>= \s -> guard (s == "nil") $> ENil

pInt :: Parser Expr
pInt = EInt <$> L.signed space L.decimal

pBool :: Parser Expr
pBool = EBool <$> (symbol "#t" $> True <|> symbol "#f" $> False)

pSymbol :: Parser Expr
pSymbol = ESymbol <$> lexeme (T.cons <$> start <*> takeSymbol)
 where
  start = P.letterChar <|> P.satisfy (`elem` ("+-_^*/<=>" :: String))

pList :: Parser Expr
pList = EList <$> P.choice
  [ (:) <$> (symbol "'" $> ESymbol "quote") <*> list  -- '(quoted-list)
  , list
  ]
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
