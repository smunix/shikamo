module Shikamo.Lexer.Loc ( Loc(..)
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
