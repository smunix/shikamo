module Shikamo.Parse.Parser ( appExpr
                            , termExpr
                            , grammarExpr
                            , lamExpr
                            , varExpr
                            ) where

import           Shikamo.Lang.Expr   as Shikamo
import           Shikamo.Parse.Lexer as Shikamo

import qualified Data.Text.Lazy      as Text
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Expr    as Parsec
import qualified Text.Parsec.Token   as Parsec

data Bind where
  Bind :: String -> Expr -> Bind
  deriving (Eq, Show)

infixOp :: (Monad m) => String -> (a -> a -> a) -> Parsec.Assoc -> Op m a
infixOp str op = Parsec.Infix (reservedOp str >> return op)

prefixOp :: (Monad m) => String -> (a -> a) -> Op m a
prefixOp str op = Parsec.Prefix (reservedOp str >> return op)

arithOps :: (Monad m) => Operators m Expr
arithOps = [ [ prefixOp "-" (UnaOp Neg)
             , prefixOp "not" (UnaOp Not)
             ]
           , [ infixOp "*" (BinOp Mul) Parsec.AssocLeft
             , infixOp "/" (BinOp Div) Parsec.AssocLeft
             ]
           , [ infixOp "+" (BinOp Add) Parsec.AssocLeft
             , infixOp "-" (BinOp Sub) Parsec.AssocLeft
             ]
           , [ infixOp ">" (BinOp Gt) Parsec.AssocLeft
             , infixOp ">=" (BinOp Gte) Parsec.AssocLeft
             , infixOp "<" (BinOp Lt) Parsec.AssocLeft
             , infixOp "<=" (BinOp Lte) Parsec.AssocLeft
             , infixOp "==" (BinOp Eq) Parsec.AssocLeft
             , infixOp "!=" (BinOp Neq) Parsec.AssocLeft
             ]
           , [ infixOp "and" (BinOp And) Parsec.AssocLeft
             , infixOp "or" (BinOp Or) Parsec.AssocLeft
             ]
           ]

-- | variable : a
varExpr :: (Monad m) => Parser m Expr
varExpr = do
  i <- identifier
  return $ Var i

-- | lambda abstraction : \x y z.e
--   rewritten as : \x.\y.\z.e
lamExpr :: (Monad m) => Parser m Expr
lamExpr = do
  reserved "\\"
  args <- Parsec.many1 identifier
  reservedOp "."
  body <- appExpr
  return $ foldr Lam body args

-- | application of expressions in the form : e1 e2 e3 .. eN
--   rewritten as : App (App (App (App e1 e2) e3) .. eN-1) eN
appExpr :: (Monad m) => Parser m Expr
appExpr = do
  exprs <- Parsec.many1 termExpr
  return $ foldl1 App exprs

grammarExpr :: (Monad m) => Parser m Expr
grammarExpr
  = parens appExpr
  Parsec.<|> varExpr
  Parsec.<|> lamExpr

termExpr :: (Monad m) => Parser m Expr
termExpr = Parsec.buildExpressionParser arithOps grammarExpr
