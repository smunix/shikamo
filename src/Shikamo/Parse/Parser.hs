module Shikamo.Parse.Parser ( Parser(..)
                            , TyUnkind(..)
                            , parse
                            , parseTest
                            , dataDecl
                            , varFnDeclExpl
                            , expr
                            ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as M
import           Data.Kind           (Constraint)
import           Data.List           (foldl', nub)
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

import           Shikamo.Lang.Expr   as E
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
type Parser a = forall s m i . (i ~ Int, Integral i, Monad m, P.Stream s m (Lex Tok)) => P.ParsecT s i m a
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

data TyUnkind a where
  TyUnkindCtor :: a           -> TyUnkind a
  TyUnkindVar  :: a            -> TyUnkind a
  TyUnkindApp  :: TyUnkind a -> TyUnkind a -> TyUnkind a
  deriving (Eq, Show, Generic, Data, Typeable)

parens :: Parser a -> Parser a
parens p = go <?> "parens e.g. (x)"
  where
    go = do
      isTok OpenParenTok
      e <- p <?> "expression inside parentheses e.g. (foo)"
      isTok CloseParenTok
      return e

isTok :: Tok -> Parser (Located Tok)
isTok tok = satisfyParse (== tok)

varParser :: (i ~ Id) => Parser i
varParser = locatedTok <$> varLocParser

varLocParser :: (i ~ Id) => Parser (Located i)
varLocParser = (do
                   consumeWithParse (\case
                                        (Lex (VarTok txt) loc) -> Just $ Located (Id txt) loc
                                        _ -> Nothing)
               ) <?> "type variable (e.g. 'a', 's', etc.)"

ctorParser :: (i ~ Id) => Parser i
ctorParser = locatedTok <$> ctorLocParser

ctorLocParser :: (i ~ Id) => Parser (Located i)
ctorLocParser = consumeWithParse (\case
                                     Lex (CtorTok txt) loc -> Just $ Located (Id txt) loc
                                     _ -> Nothing
                                   ) <?> "value constructor (e.g. Just, Nothing)"

parseAST :: Parser [Decl TyUnkind Id Loc]
parseAST = parseModule <* endOfParse

-- | data Data = Ctor ...
dataDecl :: (t ~ TyUnkind, i ~ Id) => Parser (Located (DataType t i))
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
        locatedTok :: (i ~ Id, t ~ TyUnkind) => DataType t i
        locatedTok = DataType{..}

      return Located{..}
        where
          dataCtorP :: (i ~ Id, t ~ TyUnkind) => Parser (Located (DataCtor t i))
          dataCtorP = do
            (Located dtcName locatedLoc) <- ctorLocParser
            dtcFields <- P.many ctorSlot
            let
              locatedTok = DataCtor{..}
            return Located {..}
              where
                ctorSlot ::  (i ~ Id, t ~ TyUnkind) => Parser (t i)
                ctorSlot = (TyUnkindCtor <$> ctorParser) <|> (TyUnkindVar <$> varParser) <|> parens tyUnkindParse

data TyParsed i where
  TyParsedCtor  :: i                      -> TyParsed i
  TyParsedVar   :: i                      -> TyParsed i
  TyParsedApp   :: TyParsed i             -> TyParsed i -> TyParsed i
  TyParsedQual  :: [Predicate TyParsed i] -> TyParsed i -> TyParsed i
  TyParsedTuple :: [TyParsed i]           -> TyParsed i

tyParsed :: (i ~ Id, t ~ TyParsed) => Parser (t i)
tyParsed = infix' <|> app <|> unambiguous
  where
    infix' :: (i ~ Id, t ~ TyParsed) => Parser (t i)
    infix' = do
      lhs <- (app <|> unambiguous) <?> "lhs of function arrow. e.g. lhs -> b, where lhs := (C1 i11 i12, C2 i21 i22) => a | a ->"
      arrTok <- (fmap Just opFunArr) <|> (fmap Just opConstraintArr) <|> return Nothing
      case arrTok of
        Just (Located (RightArrowTok) _) -> do
          rhs <- tyParsed <?> "rhs of function arrow. e.g. rhs, where rhs := a | -> (a -> (b -> c))"
          return $ TyParsedApp (TyParsedApp (TyParsedCtor (Id "(->)"))  lhs) (rhs)

        Just (Located (ImplyTok) _) -> do
          lhs' <- tyParsedToPredicates lhs <?> "constraints e.g. Show a or (Read a, Show a)"
          rhs <- tyParsed <?> "rhs of constraints"
          return $ TyParsedQual lhs' rhs

        _ -> return lhs

    tyParsedToPredicates :: (i ~ Id, t ~ TyParsed) => t i -> Parser [Predicate t i]
    tyParsedToPredicates = \case
      TyParsedTuple xs -> mapM toPredicate xs
      x -> fmap return (toPredicate x)

    toPredicate :: (i ~ Id, t ~ TyParsed) => t i -> Parser (Predicate t i)
    toPredicate t = case targs t of
      (TyParsedCtor i, vars@(_:_)) -> return (IsIn i vars)
      _ -> P.unexpected "non-class constraint"

    targs ::  (i ~ Id, t ~ TyParsed) => t i -> (t i, [t i])
    targs t = go t []
      where
        go (TyParsedApp f x) args = go f (x:args)
        go f args = (f, args)

    app :: (i ~ Id, t ~ TyParsed) => Parser (t i)
    app = do
      fn <- unambiguous
      args <- P.many unambiguous
      return $ foldl' TyParsedApp fn args

    unambiguous :: (i ~ Id, t ~ TyParsed) => Parser (t i)
    unambiguous = atomicType <|> parensTy (do
                                              xs <- P.sepBy1 tyParsed (isTok CommaTok)
                                              case xs of
                                                [x] -> return x
                                                _ -> return $ TyParsedTuple xs
                                          )

    atomicType :: (i ~ Id, t ~ TyParsed) => Parser (t i)
    atomicType = (TyParsedVar <$> varParser) <|> (TyParsedCtor <$> ctorParser)

    parensTy :: (i ~ Id, t ~ TyParsed) => Parser (t i) -> Parser (t i)
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

tyUnkindParse :: (i ~ Id, t ~ TyUnkind) => Parser (t i)
tyUnkindParse = tyParsed >>= toTyUnkind

toTyUnkind :: (i ~ Id, tp ~ TyParsed, un ~ TyUnkind) => tp i -> Parser (un i)
toTyUnkind tpx =
  case tpx of
    TyParsedCtor i -> return  $ TyUnkindCtor i
    TyParsedVar i -> return $ TyUnkindVar i
    TyParsedApp f x -> TyUnkindApp <$> (toTyUnkind f) <*> (toTyUnkind x)
    TyParsedQual {} -> P.unexpected "qualification contexted"
    TyParsedTuple {} -> P.unexpected "tuple"

varFunDecl :: Parser (Decl TyUnkind Id Loc)
varFunDecl = go <?> "variable declaration (e.g. x = 1, f = \\x -> x * x)"
  where
    go = do
      (v, itbLabel) <- consumeWithParse (\case
                                            Lex (VarTok txt) l -> Just (txt, l)
                                            _ -> Nothing) <?> "variable name"
      isTok EqualsTok <?> "`=` for variable declaration e.g. x = 1"
      e <- expr
      _ <- (return () <* isTok NonIndentedNewline) <|> endOfParse
      let
        itbId = (Id v, itbLabel)
        itbAlts = [makeAlt itbLabel e]
      return $ BindDecl itbLabel (ImplBinding (ImplTypedBinding{..}))

varFnDeclExpl :: Parser (Decl TyUnkind Id Loc)
varFnDeclExpl = go <?> "explicitly typed variable declaration (e.g. e :: Int and x = 1)"
  where
    go = do
      (Located v etbLabel) <- varLocParser <?> "variable name"
      (Lex tok _) <- anyParse <?> quotes "::" ++ " or " ++ quotes "="
      case tok of
        ColonsTok -> do
          etbScheme <-schemeParse <?> "type signature e.g. foo :: Int"
          _ <- (return () <* isTok NonIndentedNewline) <|> endOfParse
          (Located v' loc') <- varLocParser <?> "variable name"
          when (v /= v') (P.unexpected "variable binding name different to the type signature")
          _ <- isTok EqualsTok <?> quotes "=" ++  "for variable declariation e.g. x = 1"
          e <- expr
          _ <- (return () <* isTok NonIndentedNewline) <|> endOfParse
          let
            etbId = (v, etbLabel)
            etbAlts = [makeAlt etbLabel e]
          return $ BindDecl etbLabel (ExplBinding (ExplTypedBinding{..}))
        EqualsTok -> do
          e <- expr
          _ <- (return () <* isTok NonIndentedNewline) <|> endOfParse
          let
            itbLabel = etbLabel
            itbId    = (v, itbLabel)
            itbAlts  = [makeAlt itbLabel e]
          return $ BindDecl itbLabel (ImplBinding (ImplTypedBinding{..}))
        t -> P.unexpected $ quotes (show t)

kindableTpVars :: (i ~ Id) => Parser (Located (TypeVar i))
kindableTpVars = (unkinded <|> kinded) <?> "type variable (e.g. 'a', 'b', etc.)"
  where
    kinded :: (i ~ Id) => Parser (Located (TypeVar i))
    kinded = parens (do
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
                                                (Lex (VarTok txt) loc) -> Just (Located (Id txt) loc)
                                                _ -> Nothing) <?> "variable name (e.g. 'a', 'b', etc.)"
                   let
                     tvKind = StarKind
                     locatedTok = TypeVar{..}
                   return Located{..}
               )

schemeParse :: Parser (Scheme TyUnkind Id TyUnkind)
schemeParse = do
  expl <- (const True) <$> (P.lookAhead (isTok ForallTok)) <|> return False
  if expl
    then  quantified
    else do
    ty@(Qualified _ qualType) <- qualifiedParse
    return $ Forall (collectTyVars qualType) ty
    where
      quantified = do
        isTok ForallTok
        vars <- P.many1 (locatedTok <$> kindableTpVars) <?> "type variables"
        isTok PeriodTok
        ty <- qualifiedParse
        return $ Forall vars ty

      qualifiedParse :: (t ~ TyUnkind, i ~ Id) => Parser (Qualified t i (t i))
      qualifiedParse = do
        tyP <- tyParsed
        (case tyP of
           TyParsedQual preds ty -> Qualified <$> (mapM toTyUnkindPred preds) <*> (toTyUnkind ty)
             where
               toTyUnkindPred (IsIn c ts) = IsIn c <$> (mapM toTyUnkind ts)
           _ -> do
             Qualified [] <$> toTyUnkind tyP
          )

collectTyVars :: (t ~ TyUnkind, i ~ Id) => t i -> [TypeVar i]
collectTyVars = \case
  TyUnkindCtor {} -> []
  TyUnkindVar  tvId -> let tvKind = StarKind in [TypeVar{..}]
  TyUnkindApp  f x -> nub (collectTyVars f ++ collectTyVars x)

expr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
expr = case' <|> lambda <|> ifThenElse <|> infix' <|> app <|> atomicExpr
  where
    infix' :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    infix' = go <?> "infix expression (e.g. x * y)"
      where
        go = do
          lhs <- (app <|> unambiguousExpr) <?> "lhs of operator"
          op <- Just <$> opTok <|> return Nothing
          case op of
            Just (Located (OpTok txt) vLoc) -> do
              rhs <- (app <|> unambiguousExpr) <?> ("rhs of operator " <> quotes (T.unpack txt))
              badOp <- Just <$> (P.lookAhead opTok) <|> return Nothing
              let
                infixE = InfixExpr (fst . locate $ lhs) lhs (txt, VarExpr vLoc (Id txt)) rhs
              case badOp of
                Just o -> P.unexpected (show o <> " <=> " <> show infixE)
                Nothing -> return infixE
            _ -> return lhs
            where
              opTok :: Parser (Located Tok)
              opTok = satisfyParse (\case
                                       OpTok{} -> True
                                       _       -> False
                                   ) <?> "infix operator"

    case' :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    case' = do
      u <- P.getState
      Located _ loc@Loc{..} <- isTok CaseTok
      P.setState (P.sourceColumn locStart)
      e <- expr <?> "expression do do case analysis e.g. case e of ..."
      isTok OfTok
      P.lookAhead altPat <?> "case pattern"
      alts <- P.many (caseAlt (Just e) u)
      P.setState u
      return $ CaseExpr loc e alts

    altPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
    altPat = varPat <|> intLitPat <|> ctorPat <|> stringLitPat
      where
        innerPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        innerPat = parensPat <|> varPat <|> intLitPat <|> unaryCtorPat

        parensPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        parensPat = go
          where
            go = do
              isTok OpenParenTok
              p <- varPat <|> altPat
              isTok CloseParenTok
              return p

        varPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        varPat = go <?> "variable pattern (e.g. x)"
          where
            go = do
              (txt, l) <- consumeWithParse (\case
                                               Lex (VarTok txt) l -> Just (txt, l)
                                               _ -> Nothing
                                           )
              return $ if "_" `T.isPrefixOf` txt && T.length txt == 1
                       then  WildcardPat l (Id txt)
                       else  VarPat l (Id txt)

        intLitPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        intLitPat = go <?> "integer (e.g. 42, 1234)"
          where
            go = do
              (i, l) <- consumeWithParse (\case
                                             Lex (IntTok i) l -> Just (i, l)
                                             _ -> Nothing
                                         )
              return $ LitPat l (IntLit i)

        ctorPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        ctorPat = go <?> "n-ary constructor (e.g. Just x)"
          where
            go = do
              (txt, l) <- consumeWithParse (\case
                                               Lex (CtorTok txt) l -> Just (txt, l)
                                               _ -> Nothing
                                           )
              args <- P.many innerPat
              return $ CtorPat l (Id txt) args

        stringLitPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        stringLitPat = go <?> "string (e.g. \"foo\", \"bar\")"
          where
            go = do
              (txt, l) <- consumeWithParse (\case
                                               Lex (StringTok txt) l -> Just (txt, l)
                                               _ -> Nothing
                                           )
              return $ LitPat l (StringLit txt)

        unaryCtorPat :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
        unaryCtorPat = go <?> "unary constructor (e.g. Nothing)"
          where
            go = do
              (txt, l) <- consumeWithParse (\case
                                               Lex (CtorTok txt) l -> Just (txt, l)
                                               _ -> Nothing
                                           )
              return $ CtorPat l (Id txt) []

    caseAlt :: (t ~ TyUnkind, i ~ Id, l ~ Loc, u ~ Int) => Maybe (Expr t i l) -> u -> Parser (CaseAlt t i l)
    caseAlt m startCol = go <?> "case alternative " <> (case m of
                                                   Just e -> "e.g.\n\ncase " <> show e <> " of\n  Just bar -> bar"
                                                   Nothing -> ""
                                                )
      where
        go = do
          u <- P.getState
          caseAltPattern <- altPat
          when
            ((P.sourceColumn . locStart . fst . locate) caseAltPattern /= startCol)
            (P.unexpected $
              "pattern at column "
              <> show ((P.sourceColumn . locStart . fst . locate) caseAltPattern)
              <> ", it should start at column "
              <> show startCol
              <> " to match the others"
            )
          P.setState startCol
          isTok RightArrowTok
          caseAltExpr <- expr
          P.setState u
          let
            caseAltLabel = fst $ locate caseAltPattern
          return CaseAlt{..}

    lambda :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    lambda = go <?> "lambda expression (e.g. \\x -> x)"
      where
        go = do
          Located _ loc <- isTok BackslashTok <?> "lambda expression (e.g. \\x -> x)"
          args <- P.many1 funcParam <?> "lambda params"
          isTok RightArrowTok <|> isTok PeriodTok
          e <- expr
          return $ LambExpr loc (E.Alt loc args e)

    funcParam :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Pattern t i l)
    funcParam = go <?> "function parameter (e.g. 'x', 'limit', etc.)"
      where
        go = do
          (Located i@(Id v) loc) <- varLocParser
          return $ VarPat loc i

    ifThenElse :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    ifThenElse = go <?> "if expression (e.g. 'if c then t else f')"
      where
        go = do
          Located _ loc <- isTok IfTok
          c <- expr <?> "conditional expr in if-expression"
          isTok ThenTok
          t <- expr <?> "then/true expr in if-expression"
          isTok ElseTok
          f <- expr <?> "else/false expr in if-expression"
          return $ IfExpr loc c t f

    app :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    app = do
      f <- funcOp <?> "fun expression"
      xs <- P.many unambiguousExpr <?> "fun arguments"
      case xs of
        [] -> return f
        _ -> return $ foldl (AppExpr (fst $ locate f)) f xs

    atomicExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    atomicExpr = varExpr <|> charExpr <|> stringExpr <|> intExpr <|> decExpr <|> ctorExpr

    varExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    varExpr = go <?> "variable (e.g. a, b, _a)"
      where
        go = do
          (Located i@(Id v) l) <- varLocParser
          return $ if  "_" `T.isPrefixOf` v
                   then ConstExpr l i
                   else VarExpr l i

    charExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    charExpr = go <?> "character (e.g. 'a')"
      where
        go = do
          (c, l) <- consumeWithParse (\case
                                         Lex (CharTok c) l -> Just (c, l)
                                         _ -> Nothing
                                     )
          return $ LitExpr l (CharLit c)

    stringExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    stringExpr = go <?> "string (e.g. \"abc\")"
      where
        go = do
          (txt, l) <- consumeWithParse (\case
                                           Lex (StringTok txt) l -> Just (txt, l)
                                           _ -> Nothing
                                       )
          return $ LitExpr l (StringLit txt)

    intExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    intExpr =  go <?> "integer (e.g. 123)"
      where
        go = do
          (i, l) <- consumeWithParse (\case
                                         Lex (IntTok i) l -> Just (i, l)
                                         _ -> Nothing
                                     )
          return $ LitExpr l (IntLit i)

    decExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    decExpr =   go <?> "decimal (e.g. 123.45)"
      where
        go = do
          (d, l) <- consumeWithParse (\case
                                         Lex (FloatTok d) l -> Just (d, l)
                                         _ -> Nothing
                                     )
          return $ LitExpr l (RatioLit $ realToFrac d)

    ctorExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    ctorExpr =  go <?> "Constructor (e.g. Just, Nothing, etc.)"
      where
        go = do
          (Located i@(Id c) l) <- ctorLocParser
          return $ ConstExpr l i

    unambiguousExpr :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    unambiguousExpr = (parens expr) <|> atomicExpr

    funcOp :: (t ~ TyUnkind, i ~ Id, l ~ Loc) => Parser (Expr t i l)
    funcOp = varExpr <|> ctorExpr <|> (parens expr)

makeAlt :: l -> Expr t i l -> E.Alt t i l
makeAlt altLabel altExprs = case altExprs of
  LambExpr _ alt -> alt
  _ -> E.Alt {..}
    where
      altPatterns = []

parseModule :: Parser  [Decl TyUnkind Id Loc]
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
