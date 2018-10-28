module Shikamo.Parse.Parser ( Parser(..)
                            , UnkindedTp(..)
                            , parse
                            , parseTest
                            , dataDecl
                            ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.Kind           (Constraint)
import           Data.List           (foldl')
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

parseTest :: (Monad m, P.Stream [Lex Tok] m (Lex Tok)) => Parser a -> P.SourceName -> [Lex Tok] -> m (Either P.ParseError a)
parseTest p fp lxs = parse p fp lxs

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
  UnkindedTpCtor :: a           -> UnkindedTp a
  UnkindedTpVar  :: a            -> UnkindedTp a
  UnkindedTpApp  :: UnkindedTp a -> UnkindedTp a -> UnkindedTp a
  deriving (Eq, Show, Generic, Data, Typeable)

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

varParser :: (i ~ Id) => Parser i
varParser = (do
                consumeWithParse (\case
                                     (Lex (VarTok txt) _) -> Just (Id $ T.unpack txt)
                                     _ -> Nothing)
            ) <?> "type variable (e.g. 'a', 's', etc.)"

ctorParser :: (i ~ Id) => Parser i
ctorParser = locatedTok <$> ctorLocParser

ctorLocParser :: (i ~ Id) => Parser (Located i)
ctorLocParser = consumeWithParse (\case
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
      dtName <- ctorParser <?> "new type name (e.g. Maybe)"
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
          kindableTpVars :: (i ~ Id) => Parser (Located (TypeVar i))
          kindableTpVars = (unkinded <|> kinded) <?> "type variable (e.g. 'a', 'b', etc.)"
            where
              kinded :: (i ~ Id) => Parser (Located (TypeVar i))
              kinded = parensParse (do
                                       (Located (TypeVar tvId _) locatedLoc) <- unkinded
                                       isTok (ColonsTok)
                                       (Located tvKind _) <- kindParser
                                       let
                                         locatedTok = TypeVar{..}
                                       return Located{..}
                                   )
                where
                  kindParser :: Parser (Located Kind)
                  kindParser = go
                    where
                      go = do
                        kns <- P.sepBy1 (do
                                            consumeWithParse (\case
                                                                 (Lex (CtorTok "Type") loc) -> Just (Located StarKind loc)
                                                                 _ -> Nothing
                                                                 ) <?> "`Type` kind") (isTok RightArrowTok)

                        let
                          (Located _ loc) = head $ kns
                        return $ foldr1 (\(Located knl locl) (Located knr locr) -> Located (knl `FunKind` knr) locl) kns

              unkinded :: (i ~ Id) => Parser (Located (TypeVar i))
              unkinded = (do
                             (Located tvId locatedLoc) <- consumeWithParse (\case
                                                          (Lex (VarTok txt) loc) -> Just (Located (Id $ T.unpack txt) loc)
                                                          _ -> Nothing) <?> "variable name (e.g. 'a', 'b', etc.)"
                             let
                               tvKind = StarKind
                               locatedTok = TypeVar{..}
                             return Located{..}
                         )

          dataCtorP :: (i ~ Id, t ~ UnkindedTp) => Parser (Located (DataCtor t i))
          dataCtorP = do
            (Located dtcName locatedLoc) <- ctorLocParser
            dtcFields <- P.many ctorSlot
            let
              locatedTok = DataCtor{..}
            return Located {..}
              where
                ctorSlot ::  (i ~ Id, t ~ UnkindedTp) => Parser (t i)
                ctorSlot = (UnkindedTpCtor <$> ctorParser) <|> (UnkindedTpVar <$> varParser) <|> parensParse typeParser

data TypeParsed i where
  TpCtorP  :: i                        -> TypeParsed i
  TpVarP   :: i                        -> TypeParsed i
  TpAppP   :: TypeParsed i             -> TypeParsed i -> TypeParsed i
  TpQualP  :: [Predicate TypeParsed i] -> TypeParsed i -> TypeParsed i
  TpTupleP :: [TypeParsed i]          -> TypeParsed i

typeParsed :: (i ~ Id, t ~ TypeParsed) => Parser (t i)
typeParsed = infix' <|> app <|> unambiguous
  where
    infix' :: (i ~ Id, t ~ TypeParsed) => Parser (t i)
    infix' = do
      lhs <- (app <|> unambiguous) <?> "lhs of function arrow. e.g. lhs -> b, where lhs := (C1 i11 i12, C2 i21 i22) => a | a ->"
      arrTok <- (fmap Just opFunArr) <|> (fmap Just opConstraintArr) <|> return Nothing
      case arrTok of
        Just (Located (RightArrowTok) _) -> do
          rhs <- typeParsed <?> "rhs of function arrow. e.g. rhs, where rhs := a | -> (a -> (b -> c))"
          return $ TpAppP (TpAppP (TpCtorP (Id "(->)"))  lhs) (rhs)

        Just (Located (ImplyTok) _) -> do
          lhs' <- tpParsedToPredicates lhs <?> "constraints e.g. Show a or (Read a, Show a)"
          rhs <- typeParsed <?> "rhs of constraints"
          return $ TpQualP lhs' rhs

        _ -> return lhs

    tpParsedToPredicates :: (i ~ Id, t ~ TypeParsed) => t i -> Parser [Predicate t i]
    tpParsedToPredicates = \case
      TpTupleP xs -> mapM toPredicate xs
      x -> fmap return (toPredicate x)

    toPredicate :: (i ~ Id, t ~ TypeParsed) => t i -> Parser (Predicate t i)
    toPredicate t = case targs t of
      (TpCtorP i, vars@(_:_)) -> return (IsIn i vars)
      _ -> P.unexpected "non-class constraint"

    targs ::  (i ~ Id, t ~ TypeParsed) => t i -> (t i, [t i])
    targs t = go t []
      where
        go (TpAppP f x) args = go f (x:args)
        go f args = (f, args)

    app :: (i ~ Id, t ~ TypeParsed) => Parser (t i)
    app = do
      fn <- unambiguous
      args <- P.many unambiguous
      return $ foldl' TpAppP fn args

    unambiguous :: (i ~ Id, t ~ TypeParsed) => Parser (t i)
    unambiguous = atomicType <|> parensTy (do
                                              xs <- P.sepBy1 typeParsed (isTok CommaTok)
                                              case xs of
                                                [x] -> return x
                                                _ -> return $ TpTupleP xs
                                          )

    atomicType :: (i ~ Id, t ~ TypeParsed) => Parser (t i)
    atomicType = (TpVarP <$> varParser) <|> (TpCtorP <$> ctorParser)

    parensTy :: (i ~ Id, t ~ TypeParsed) => Parser (t i) -> Parser (t i)
    parensTy p = go <?> "parentheses e.g. (T a)"
      where
        go = do
          isTok OpenParenTok
          t <- p <?> "type inside parentheses e.g. (Maybe a)"
          isTok CloseParenTok
          return t

    opFunArr :: Parser (Located Tok)
    opFunArr = isTok RightArrowTok

    opConstraintArr :: Parser (Located Tok)
    opConstraintArr = isTok ImplyTok

typeParser :: (i ~ Id, t ~ UnkindedTp) => Parser (t i)
typeParser = typeParsed >>= toUnkinded
  where
    toUnkinded :: (i ~ Id, tp ~ TypeParsed, un ~ UnkindedTp) => tp i -> Parser (un i)
    toUnkinded tpx =
      case tpx of
        TpCtorP i -> return  $ UnkindedTpCtor i
        TpVarP i -> return $ UnkindedTpVar i
        TpAppP l r -> UnkindedTpApp <$> (toUnkinded l) <*> (toUnkinded r)
        TpQualP {} -> P.unexpected "qualification contexted"
        TpTupleP {} -> P.unexpected "tuple"

varFnDeclExpl = undefined

parseModule :: Parser  [Decl UnkindedTp Id Loc]
parseModule = P.many ( varFnDeclExpl <|> (uncurry DataDecl . locate) <$> dataDecl)

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
