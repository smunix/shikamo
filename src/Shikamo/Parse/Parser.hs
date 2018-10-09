module Shikamo.Parse.Parser ( appExpr
                            , arithExpr
                            , compareExpr
                            , computationExpr
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

{-
type Str = Text.Text
type Parser m a    = Parsec.ParsecT Text.Text () m a
type Op m a        = Parsec.Operator Text.Text () m a
type Operators m a = Parsec.OperatorTable Text.Text () m a
-}

data Bind where
  Bind :: String -> Expr -> Bind
  deriving (Eq, Show)

infixOp :: (Monad m) => String -> (a -> a -> a) -> Parsec.Assoc -> Op m a
infixOp str op = Parsec.Infix (reservedOp str >> return op)

prefixOp :: (Monad m) => String -> (a -> a) -> Op m a
prefixOp str op = Parsec.Prefix (reservedOp str >> return op)

arithOps :: (Monad m) => Operators m Expr
arithOps = [ [ prefixOp "-" (UnaOp Neg)
             ]
           , [ infixOp "*" (BinOp Mul) Parsec.AssocLeft
             , infixOp "/" (BinOp Div) Parsec.AssocLeft
             ]
           , [ infixOp "+" (BinOp Add) Parsec.AssocLeft
             , infixOp "-" (BinOp Sub) Parsec.AssocLeft
             ]
           ]

compareOps :: (Monad m) => Operators m Expr
compareOps = [ [ prefixOp "not" (UnaOp Not)
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
  -- let
  --   term = (arithExpr Parsec.<|> compareExpr)

  exprs <- Parsec.many1 grammarExpr
  return $ foldl1 App exprs

grammarExpr :: (Monad m) => Parser m Expr
grammarExpr
  = parens term
  Parsec.<|> varExpr
  Parsec.<|> lamExpr
  Parsec.<|> appExpr
  where
    term = arithExpr Parsec.<|> compareExpr

arithExpr :: (Monad m) => Parser m Expr
arithExpr = Parsec.buildExpressionParser arithOps grammarExpr

compareExpr :: (Monad m) => Parser m Expr
compareExpr = Parsec.buildExpressionParser compareOps grammarExpr

computationExpr :: (Monad m) => Parser m Expr
computationExpr = Parsec.buildExpressionParser (arithOps <> compareOps) grammarExpr
