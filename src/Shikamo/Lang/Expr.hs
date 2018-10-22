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
                         , BindGroup(..)
                         , CaseAlt(..)
                         , Class(..)
                         , DataTypeCtor(..)
                         , ExplTypedBinding(..)
                         , Expr(..)
                         , Id(..)
                         , ImplTypedBinding(..)
                         , Kind(..)
                         , Lit(..)
                         , Loc(..)
                         , Pattern(..)
                         , Predicate(..)
                         , Qualified(..)
                         , Scheme(..)
                         , emptyLoc
                         ) where

import           Control.DeepSeq
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.String
import           Data.Text           (Text)
import           GHC.Generics
import           Text.Parsec         (ParseError)
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error   as P
import qualified Text.Parsec.Pos     as P

-- | A Location annotating 'Lex' elements.
data Loc where
  Loc :: { locStart :: !P.SourcePos
         , locEnd :: !P.SourcePos
         } -> Loc
  deriving (Show, Eq, Generic, Data, Typeable)

setFilepath :: Loc -> P.SourceName -> Loc
setFilepath (Loc s e) fp = Loc {..}
  where
    locStart = P.setSourceName s fp
    locEnd   = P.setSourceName e fp

-- | An empty initial location
emptyLoc = Loc{..}
  where
    locStart = P.initialPos "<null>"
    locEnd   = P.initialPos "<null>"

-- * Expression AST

data Kind where
  StarKind :: Kind
  FunKind :: Kind -> Kind -> Kind
  deriving (Typeable, Show, Eq, Ord, Data, Generic)

data TypeVar i where
  TypeVar :: { tvId :: i
             , tvKind :: !Kind
             } -> TypeVar i
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A data type constructor.
data DataTypeCtor t i where
  DataTypeCtor :: { dtcName :: i
                  , dtcFields :: [t i]
                  } -> DataTypeCtor t i
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A data type.
data DataType t i where
  DataType :: { dtName :: i
              , dtVars :: [TypeVar i]
              , dtCtors :: [DataTypeCtor t i]
              } -> DataType t i
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A BindGroup represents bindings as in a @LetExpr@
data BindGroup t i l where
  BindGroup :: { bgExplTypedBindings :: ![ExplTypedBinding t i l]
               , bgImplTypedBindings :: ![ImplTypedBinding t i l]
               } -> BindGroup t i l
  deriving (Show, Generic, Data, Typeable, Functor, Foldable, Traversable, Eq)

-- | A Predicate type that has an @id@ and a @type list@
data Predicate t i where
  IsIn :: i -> [t i] -> Predicate t i
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A Qualified type has a list of @Predicate@ to satisfy
data Qualified t i typ where
  Qualified :: { qualPredicates :: ![Predicate t i]
               , qualType       :: !typ
               } -> Qualified t i typ
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A type constructor.
data TypeCtor i where
  TypeCtor :: { typeCtorId   :: !i
              , typeCtorKind :: !Kind
              } -> TypeCtor i
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A type scheme.
data Scheme t i typ where
  Forall :: [TypeVar i] -> Qualified t i (typ i) -> Scheme t i typ
  deriving (Show, Eq, Generic, Data, Typeable)

data CaseAlt t i l where
  CaseAlt :: { caseAltLabel   :: l
             , caseAltPattern :: Pattern t i l
             , caseAltExpr    :: Expr t i l
             } -> CaseAlt t i l
  deriving (Show, Generic, Data, Typeable, Functor, Foldable, Traversable, Eq)

-- | A pattern match
data Pattern t i l where
  VarPattern      :: Pattern t i l
  WildcardPattern :: Pattern t i l
  AsPattern       :: Pattern t i l
  LitPattern      :: Pattern t i l
  CtorPattern     :: Pattern t i l
  deriving (Show, Generic, Data, Typeable, Functor, Foldable, Traversable, Eq)

-- | An Identifier as used in @Shikamo@
newtype Id = Id { idString :: String }
  deriving (Show, IsString, Ord, Generic, Data, Typeable, Eq)

-- | Type for supported Literals.
data Lit where
  IntLit    :: Lit
  CharLit   :: Lit
  RatioLit  :: Lit
  StringLit :: Lit
  deriving (Show, Generic, Data, Typeable, Eq)

-- | A Shikamo Expression. (Totally borrowed from Haskell)
data Expr t i l where
  AppExpr    :: l -> Expr t i l -> Expr t i l -> Expr t i l
  CaseExpr   :: l -> Expr t i l -> [CaseAlt t i l] -> Expr t i l
  ConstExpr  :: l -> Id -> Expr t i l
  IfExpr     :: l -> Expr t i l -> Expr t i l -> Expr t i l -> Expr t i l
  InfixExpr  :: l -> Expr t i l -> (String, Expr t i l) -> Expr t i l -> Expr t i l
  LambExpr   :: l -> Alt t i l -> Expr t i l
  LitExpr    :: l -> Lit -> Expr t i l
  LetExpr    :: l -> BindGroup t i l -> Expr t i l -> Expr t i l
  ParensExpr :: l -> Expr t i l -> Expr t i l
  VarExpr    :: l -> i -> Expr t i l
  deriving (Show, Generic, Data, Typeable, Functor, Foldable, Traversable, Eq)

-- | An Alternative specifies the left and right hand sides of a function definition.
-- With some complete syntax for Expr, values of type Alternative might be used in the representation of
-- lambda and case exprs as well.
data Alt t i l where
  Alt :: { altLabel :: l
         , altPatterns :: ![Pattern t i l]
         , altExprs :: ![Expr t i l]
         } -> Alt t i l
  deriving (Show, Generic, Data, Typeable, Functor, Foldable, Traversable, Eq)

data ImplTypedBinding t i l where
  ImplTypedBinding :: { itbLabel :: l
                      , itbId    :: !(i, l)
                      , itbAlts  :: ![Alt t i l]
                      } -> ImplTypedBinding t i l
  deriving (Show, Generic, Data, Typeable, Functor, Traversable, Foldable, Eq)

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
  deriving (Show, Generic, Data, Typeable, Functor, Traversable, Foldable, Eq)

-- | A binding declaration
data Binding t i l where
  ImplBinding :: ImplTypedBinding t i l -> Binding t i l
  ExplBinding :: ExplTypedBinding t i l -> Binding t i l
  deriving (Show, Generic, Data, Typeable)

-- | A Dictionary for a class
data Dictionary t i l where
  Dictionary :: { dictName :: i
                , dictMethods :: M.HashMap i (l, Alt t i l)
                } -> Dictionary t i l
  deriving (Show, Eq, Generic, Data, Typeable, Functor, Traversable, Foldable)

-- | A Class instance.
data Instance (t :: * -> *) i l where
  Instance :: { instPredicate  :: !(Scheme t i (Predicate t))
              , instDictionary :: !(Dictionary t i l)
              } -> Instance t i l
  deriving (Show, Eq, Generic, Data, Typeable, Functor, Traversable, Foldable)

-- | A class.
data Class (t :: * -> *) i l where
  Class :: { classTypeVars     :: ![TypeVar i]
           , classSuperclasses :: ![Predicate t i]
           , classInstances    :: ![Instance t i l]
           , className         :: i
           , classMethods      :: M.HashMap i (Scheme t i t)
           } -> Class t i l
  deriving (Show, Eq, Generic, Data, Typeable, Functor, Traversable, Foldable)

-- | A declaration.
data Decl t i l where
  DataDecl     :: l -> DataType t i -> Decl t i l
  BindDecl     :: l -> Binding t i l -> Decl t i l
  ClassDecl    :: l -> Class t i l -> Decl t i l
  InstanceDecl :: l -> Instance t i l -> Decl t i l
  deriving (Show, Generic, Data, Typeable)
