module Shikamo.Parse.Parser ( Parser(..)
                            , parse
                            ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.Kind           (Constraint)
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
import           Shikamo.Lexer.Loc

data ParseErr
  deriving (Show, Eq, Generic, Typeable)
data LexErr
  deriving (Show, Eq, Generic, Typeable)

data Error p where
  ParserErr :: P.ParseError -> Error ParseErr
  LexerErr  :: P.ParseError -> Error LexErr
  deriving (Typeable)

deriving instance (Show p) => Show (Error p)
instance (Show p, Typeable p) => Exception (Error p)

-- | A Parser generates a stream of 'Decl' from the stream ['Lex'] produced by 'Lexer'
type Parser a = forall s m i . (Integral i, Num i, Monad m, P.Stream s m (Lex Tok)) => P.ParsecT s i m a
-- | Parse a stream [Lex] into its 'Lex' lexemes
parse :: (Monad m, P.Stream s m (Lex Tok)) => Parser a -> P.SourceName -> s -> m (Either P.ParseError a)
parse p = P.runParserT p 0

parseWith :: (P.Stream s m Char, MonadThrow m) => Parser a -> P.SourceName -> s -> m a
parseWith p fp inp = do
  l <- lexemize fp inp
  case l of
    Left e -> throwM (LexerErr e)
    Right lxs -> do
      g <- parse p fp lxs
      case g of
        Left e -> throwM (ParserErr e)
        Right a -> return a

data UnkindedTp a where
  UnkindedTp :: a -> UnkindedTp a

parensParse :: Parser a -> Parser a
parensParse p = go <?> "parens e.g. (x)"
  where
    go = do
      isTok OpenParenTok
      e <- p <?> "expression inside parentheses e.g. (foo)"
      isTok CloseParenTok
      return e

isTok :: Tok -> Parser (Located Tok)
isTok tok = satisfyParse (== tok)

ctorIdParser :: (i ~ Id) => Parser i
ctorIdParser = locatedTok <$> ctorIdLocParser

ctorIdLocParser :: (i ~ Id) => Parser (Located i)
ctorIdLocParser = consumeWithParse (\case
                                   Lex (CtorTok txt) loc -> Just $ Located (Id $ T.unpack txt) loc
                                   _ -> Nothing
                               )
        <?> "value constructor (e.g. Just, Nothing)"


parseAST :: Parser [Decl UnkindedTp Id Loc]
parseAST = parseModule <* endOfParse

-- | data Data = Ctor ...
dataDecl :: (t ~ UnkindedTp, i ~ Id) => Parser (Located (DataType t i))
dataDecl = go <?> "data declaration (e.g. data Maybe a = Just a | Nothing)"
  where
    go = do
      -- peek location
      locatedLoc <- fst . locate <$> isTok (DataTok)
      -- parse ctor
      dtName <- ctorIdParser <?> "new type name (e.g. Maybe)"
      -- type vars
      dtVars <- P.many (locatedTok <$> kindableTpVars)
      _ <- isTok EqualsTok
      dtCtors <- P.sepBy1 (locatedTok <$> dataCtorP) (isTok BarTok)
      (return () <* satisfyParse (== NonIndentedNewline)) <|> endOfParse

      let
        locatedTok :: (i ~ Id, t ~ UnkindedTp) => DataType t i
        locatedTok = DataType{..}

      return Located{..}
        where
          kindableTpVars :: Parser (Located (TypeVar i))
          kindableTpVars = undefined

          dataCtorP :: (i ~ Id, t ~ UnkindedTp) => Parser (Located (DataCtor t i))
          dataCtorP = do
            (Located dtcName locatedLoc) <- ctorIdLocParser
            dtcFields <- P.many ctorSlot
            let
              locatedTok = DataCtor{..}
            return Located {..}
              where
                ctorSlot ::  (i ~ Id, t ~ UnkindedTp) => Parser (t i)
                ctorSlot = UnkindedTp <$> (ctorIdParser <|> varParser <|> parensParse typeParser)
                  where
                    varParser, typeParser :: (i ~ Id) => Parser i
                    typeParser = undefined
                    varParser = (do
                                    consumeWithParse (\case
                                                         (Lex (VarTok txt) _) -> Just (Id $ T.unpack txt))
                                ) <?> "type variable (e.g. 'a', 's', etc.)"

varFnDeclExpl = undefined

parseModule :: Parser  [Decl UnkindedTp Id Loc]
parseModule = P.many ( varFnDeclExpl
                     <|> (uncurry DataDecl . locate) <$> dataDecl)

satisfyParse :: (Tok -> Bool) -> Parser (Located Tok)
satisfyParse fn = consumeWithParse fn'
  where
    fn' (Lex{..}) | fn lexTok = Just (Located{..})
                  | otherwise = Nothing
      where
        locatedTok = lexTok
        locatedLoc = lexLoc

endOfParse :: Parser ()
endOfParse = notFollowedBy anyParse <?> "end of input"

notFollowedBy :: Parser (Lex Tok) -> Parser ()
notFollowedBy  p = P.try ((do
                              next <- P.try p
                              P.unexpected (show next))
                           <|>
                           return ())

anyParse :: Parser (Lex Tok)
anyParse = consumeWithParse Just

consumeWithParse :: (Lex Tok -> Maybe a) -> Parser a
consumeWithParse fn = do
  u <- P.getState
  P.tokenPrim
    show
    (\_ (Lex _ Loc{..}) _ -> locStart)
    (\ lx@(Lex _ Loc{..}) -> if P.sourceColumn locStart > (fromIntegral u)
                             then
                               fn lx
                             else
                               Nothing)
