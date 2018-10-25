module Shikamo.Parse.Parser ( Parser(..)
                            , parse
                            ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Parsec         ((<?>), (<|>))
import qualified Text.Parsec         as P
import qualified Text.Parsec.Error   as P
import qualified Text.Parsec.Pos     as P
import qualified Text.Parsec.Text    as P
import           Text.Printf

import           Shikamo.Lang.Expr
import           Shikamo.Lexer.Lexer


-- | A Parser generates a stream of 'Decl' from the stream ['Lex'] produced by 'Lexer'
type Parser a = forall s m . (Monad m, P.Stream s m (Lex Tok)) => P.ParsecT s Int m a
-- | Parse a stream [Lex] into its 'Lex' lexemes
parse :: (Monad m, P.Stream s m (Lex Tok)) => Parser a -> P.SourceName -> s -> m (Either P.ParseError a)
parse p = P.runParserT p 0
