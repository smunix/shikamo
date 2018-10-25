module Shikamo.Lexer.Loc ( Loc(..)
                         , Locate(..)
                         , Located(..)
                         , emptyLoc
                         ) where

import qualified Text.Parsec         as P
import qualified Text.Parsec.Pos     as P
import           Data.Data           (Data, Typeable)
import           GHC.Generics

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

data Located tok where
  Located :: { locatedTok :: tok -- ^ get the token
             , locatedLoc :: Loc -- ^ get the location
             } -> Located tok

class Locate s a | s -> a where
  locate :: s -> (Loc, a)

instance Locate (Located t) t where
  locate (Located{..}) = (locatedLoc, locatedTok)

