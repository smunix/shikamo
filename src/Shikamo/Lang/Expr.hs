module Shikamo.Lang.Expr ( BinOp(..)
                         , Expr(..)
                         , UnaOp(..)
                         ) where

type Sym = String

-- | an Expr in a lambda calculus is one of the following:
data Expr where
  Var :: Sym -> Expr                     -- ^ Variable
  Lam :: Sym -> Expr -> Expr             -- ^ (Lambda) Abstration
  App :: Expr -> Expr -> Expr            -- ^ Application
  {- extensions to our lambda calculus -}
  UnaOp :: UnaOp -> Expr -> Expr         -- ^ Unary operation
  BinOp :: BinOp -> Expr -> Expr -> Expr -- ^ Binary operation
  {- unit to match non-sense (failures)-}
  Unit :: Expr
  deriving (Eq, Show)

-- | all Unary operations
data UnaOp where
  Neg :: UnaOp
  Not :: UnaOp
  deriving (Eq, Show)

-- | all Binary operations
data BinOp where
  {- arithmetics -}
  Add :: BinOp
  Sub :: BinOp
  Mul :: BinOp
  Div :: BinOp
  {- comparisons -}
  Gt :: BinOp
  Gte :: BinOp
  Lt :: BinOp
  Lte :: BinOp
  Eq :: BinOp
  Neq :: BinOp
  {- booleans -}
  And :: BinOp
  Or :: BinOp
  deriving (Eq, Show)

-- | a Declaration is a named Expression
data Decl where
  Decl :: String -> Expr -> Decl
  deriving (Eq, Show)

-- | a Program is a list of Declarations followed by the main Expr
data Program where
  Program :: [Decl] -> Expr -> Program
  deriving (Eq, Show)
