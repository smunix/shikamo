{-|
Module      : Shikamo.Lang.Expr
Description : Expression Abstract Syntax Tree
Copyright   : (c) Providence Salumu, 2018
License     : BSD-3
Maintainer  : Providence.Salumu@smunix.com
Stability   : experimental
Portability : POSIX

This module describes the hierarchy of the all supported @expressions@
as used by Shikamo. The end result of parsing should be a list of 'Decl'.
-}
module Shikamo.Lang.Expr ( Alt(..)
                         , Binding(..)
                         , BindGroup(..)
                         , CaseAlt(..)
                         , Class(..)
                         , DataType(..)
                         , DataCtor(..)
                         , Decl(..)
                         , ExplTypedBinding(..)
                         , Expr(..)
                         , Id(..)
                         , ImplTypedBinding(..)
                         , Kind(..)
                         , Lit(..)
                         , Pattern(..)
                         , Predicate(..)
                         , Qualified(..)
                         , Scheme(..)
                         , TypeVar(..)
                         ) where

import           Control.DeepSeq
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.Kind           (Type, Constraint)
import           Data.String
import           Data.Text           (Text)
import           GHC.Generics
import           Text.Parsec         (ParseError)
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error   as P
import qualified Text.Parsec.Pos     as P

import Shikamo.Lexer.Loc

-- * Expression AST

data Kind where
  StarKind :: Kind
  FunKind :: Kind -> Kind -> Kind
  deriving ( Typeable
           , Show
           , Eq
           , Ord
           , Data
           , Generic)

data TypeVar i where
  TypeVar :: { tvId :: i
             , tvKind :: !Kind
             } -> TypeVar i
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A data type constructor.
data DataCtor t i where
  DataCtor :: { dtcName :: i
              , dtcFields :: [t i]
              } -> DataCtor t i
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A data type.
data DataType t i where
  DataTypeUnit :: DataType t i
  DataType :: { dtName :: i
              , dtVars :: [TypeVar i]
              , dtCtors :: [DataCtor t i]
              } -> DataType t i
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A BindGroup represents bindings as in a @LetExpr@
data BindGroup t i l where
  BindGroup :: { bgExplTypedBindings :: ![ExplTypedBinding t i l]
               , bgImplTypedBindings :: ![ImplTypedBinding t i l]
               } -> BindGroup t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Foldable
           , Traversable
           , Eq)

-- | A Predicate type that has an @id@ and a @type list@
data Predicate t i where
  IsIn :: i -> [t i] -> Predicate t i
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A Qualified type has a list of @Predicate@ to satisfy
data Qualified t i typ where
  Qualified :: { qualPredicates :: ![Predicate t i]
               , qualType       :: !typ
               } -> Qualified t i typ
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A type constructor.
data TypeCtor i where
  TypeCtor :: { typeCtorId   :: !i
              , typeCtorKind :: !Kind
              } -> TypeCtor i
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A type scheme.
data Scheme t i typ where
  Forall :: [TypeVar i] -> Qualified t i (typ i) -> Scheme t i typ
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

data CaseAlt t i l where
  CaseAlt :: { caseAltLabel   :: l
             , caseAltPattern :: Pattern t i l
             , caseAltExpr    :: Expr t i l
             } -> CaseAlt t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Foldable
           , Traversable
           , Eq)

-- | A pattern match
data Pattern t i l where
  VarPat      :: l -> i -> Pattern t i l
  WildcardPat :: l -> i -> Pattern t i l
  AsPat       :: l -> Pattern t i l
  LitPat      :: l -> Lit -> Pattern t i l
  CtorPat     :: l -> i -> [Pattern t i l] -> Pattern t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Foldable
           , Traversable
           , Eq)

instance (l~Loc) => Locate (Pattern t i l) (Pattern t i l) where
  locate p@(VarPat l _)      = (l, p)
  locate p@(WildcardPat l _) = (l, p)
  locate p@(LitPat l _)      = (l, p)
  locate p@(CtorPat l _ _)   = (l, p)
  locate p@(AsPat l)         = (l, p)

-- | An Identifier as used in @Shikamo@
newtype Id = Id { idString :: Text }
  deriving ( Show
           , IsString
           , Ord
           , Generic
           , Data
           , Typeable
           , Eq)

-- | Type for supported Literals.
data Lit where
  IntLit    :: Integer -> Lit
  CharLit   :: Char -> Lit
  RatioLit  :: Rational -> Lit
  StringLit :: Text -> Lit
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Eq)

-- | A Shikamo Expression. (Totally borrowed from Haskell)
data Expr t i l where
  UnitExpr :: Expr t i l
  AppExpr    :: l -> Expr t i l -> Expr t i l -> Expr t i l
  CaseExpr   :: l -> Expr t i l -> [CaseAlt t i l] -> Expr t i l
  ConstExpr  :: l -> Id -> Expr t i l
  IfExpr     :: l -> Expr t i l -> Expr t i l -> Expr t i l -> Expr t i l
  InfixExpr  :: l -> Expr t i l -> (Text, Expr t i l) -> Expr t i l -> Expr t i l
  LambExpr   :: l -> Alt t i l -> Expr t i l
  LitExpr    :: l -> Lit -> Expr t i l
  LetExpr    :: l -> BindGroup t i l -> Expr t i l -> Expr t i l
  ParensExpr :: l -> Expr t i l -> Expr t i l
  VarExpr    :: l -> i -> Expr t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Foldable
           , Traversable
           , Eq)

instance (l ~ Loc) => Locate (Expr t i l) (Expr t i l) where
  locate e@(AppExpr l _ _)     = (l, e)
  locate e@(CaseExpr l _ _)    = (l, e)
  locate e@(ConstExpr l _)     = (l, e)
  locate e@(IfExpr l _ _ _)    = (l, e)
  locate e@(InfixExpr l _ _ _) = (l, e)
  locate e@(LambExpr l _)      = (l, e)
  locate e@(LitExpr l _)       = (l, e)
  locate e@(LetExpr l _ _)     = (l, e)
  locate e@(ParensExpr l _)    = (l, e)
  locate e@(VarExpr l _)       = (l, e)

-- | An Alternative specifies the left and right hand sides of a function definition.
-- With some complete syntax for Expr, values of type Alternative might be used in the representation of
-- lambda and case exprs as well.
data Alt t i l where
  Alt :: { altLabel :: l
         , altPatterns :: ![Pattern t i l]
         , altExprs :: !(Expr t i l)
         } -> Alt t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Foldable
           , Traversable
           , Eq)

data ImplTypedBinding t i l where
  ImplTypedBinding :: { itbLabel :: l
                      , itbId    :: !(i, l)
                      , itbAlts  :: ![Alt t i l]
                      } -> ImplTypedBinding t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Traversable
           , Foldable
           , Eq)

-- | Excplicitly typed bindings are described by the name of the fucntion taht is being defined,
-- the declared type scheme, and the list of alternatives in its definition.
--
-- Haskell requires that each Alt in the definition of a given identifier has the same number
-- of left-hand side arguments, but we are not enforcing that in Shikamo (borrowed from duet-lang)
data ExplTypedBinding t i l where
  ExplTypedBinding :: { etbLabel  :: l
                      , etbId     :: !(i, l)
                      , etbScheme :: !(Scheme t i t)
                      , etbAlts   :: ![Alt t i l]
                      } -> ExplTypedBinding t i l
  deriving ( Show
           , Generic
           , Data
           , Typeable
           , Functor
           , Traversable
           , Foldable
           , Eq)

-- | A binding declaration
data Binding t i l where
  ImplBinding :: ImplTypedBinding t i l -> Binding t i l
  ExplBinding :: ExplTypedBinding t i l -> Binding t i l
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)

-- | A Dictionary for a class
data Dictionary t i l where
  Dictionary :: { dictName :: i
                , dictMethods :: M.HashMap i (l, Alt t i l)
                } -> Dictionary t i l
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable
           , Functor
           , Traversable
           , Foldable)

-- | A Class instance.
data Instance (t :: Type -> Type) i l where
  Instance :: { instPredicate  :: !(Scheme t i (Predicate t))
              , instDictionary :: !(Dictionary t i l)
              } -> Instance t i l
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable
           , Functor
           , Traversable
           , Foldable)

-- | A class.
data Class (t :: Type -> Type) i l where
  Class :: { classTypeVars     :: ![TypeVar i]
           , classSuperclasses :: ![Predicate t i]
           , classInstances    :: ![Instance t i l]
           , className         :: i
           , classMethods      :: M.HashMap i (Scheme t i t)
           } -> Class t i l
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable
           , Functor
           , Traversable
           , Foldable)

-- | A declaration.
data Decl (t :: Type -> Type) (i :: Type) (l :: Type) where
  UnitDecl :: Decl t i l
  DataDecl     :: l -> DataType t i -> Decl t i l
  BindDecl     :: l -> Binding t i l -> Decl t i l
  ClassDecl    :: l -> Class t i l -> Decl t i l
  InstanceDecl :: l -> Instance t i l -> Decl t i l
  deriving ( Show
           , Eq
           , Generic
           , Data
           , Typeable)
