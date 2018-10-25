module Shikamo.Lexer.Lexer ( Lex(..)
                           , Lexer(..)
                           , lex
                           , lexemize
                           , Tok(..)
                           , P.Message(..)
                           , emptyLoc
                           ) where

import Prelude hiding (lex)

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Char
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
import           Shikamo.Lexer.Loc

#if 0
type Str = Text
type Parser a = forall m . (Monad m) => P.ParsecT Str () m a
#endif

-- | A lexem produced by our Token parser.
-- A @Lex@ is a typical localized token.
data Lex tok where
  Lex :: { lexTok :: tok -- ^ get the token
         , lexLoc :: Loc -- ^ get the location
         } -> Lex tok
  deriving (Show, Eq)

-- | A Token data type.
data Tok where
  UnitTok            :: Tok
  NonIndentedNewline :: Tok
  IfTok              :: Tok
  ThenTok            :: Tok
  ElseTok            :: Tok
  ClassTok           :: Tok
  WhereTok           :: Tok
  InstanceTok        :: Tok
  DataTok            :: Tok
  ForallTok          :: Tok
  CaseTok            :: Tok
  OfTok              :: Tok
  PeriodTok          :: Tok
  BackslashTok       :: Tok
  OpenParenTok       :: Tok
  CloseParenTok      :: Tok
  ImplyTok           :: Tok
  EqualsTok          :: Tok
  BarTok             :: Tok
  ColonsTok          :: Tok
  RightArrowTok      :: Tok
  CommaTok           :: Tok
  LetTok             :: Tok
  InTok              :: Tok
  OpTok              :: { opTok :: !Text } -> Tok
  CtorTok            :: { ctorTok :: !Text } -> Tok
  VarTok             :: { varTok :: !Text } -> Tok
  StringTok          :: { stringTok :: !Text } -> Tok
  CharTok            :: { charTok :: !Char } -> Tok
  IntTok             :: { intTok :: !Integer } -> Tok
  FloatTok           :: { floatTok :: !Double } -> Tok
 deriving (Show, Ord, Eq)

-- | A Lexer generates a stream of Lexems from a /String/ or /Text/ or /ByteString/
type Lexer a = forall s m . (Monad m, P.Stream s m Char) => P.ParsecT s () m a
-- | Parse a stream ([Text] | [String] | [ByteString]) into its 'Char' lexemes
lex :: (P.Stream s m Char) => Lexer a -> P.SourceName -> s -> m (Either P.ParseError a)
lex p = P.runParserT p ()

-- | Get the list of all Lexems from the imput text
lexemize :: (Monad m) =>
  FilePath -- ^ @file@ source
  -> Text -- ^ an @input@ text to parse
  -> m (Either P.ParseError [Lex Tok]) -- ^ @result@ as a list of lexem
lexemize fp t = lex lexems fp t

lexems :: Lexer [Lex Tok]
lexems = P.manyTill (P.many P.space >>= lexem) (P.try (P.spaces >> P.eof))

quotes :: String -> String
quotes t = "'" <> t <> "'"

ellipsis :: Int -> String -> String
ellipsis n str =
  if length str > 2
  then
    (take n str) ++ "..."
  else
    str
spaces1 :: Lexer ()
spaces1 = P.space >> P.spaces

atomSpace :: tok -> String -> Lexer (Lex tok)
atomSpace tok text = do
  start <- P.getPosition
  _ <- P.try ((P.string text <?> quotes text) <*
              (P.lookAhead spaces1 <?> ("space or newline after " ++ quotes text)))
  end <- P.getPosition
  return (Lex tok (Loc start end))

atom :: tok -> String -> Lexer (Lex tok)
atom tok text = do
  start <- P.getPosition
  _ <- P.try (P.string text) <?> quotes text
  end <- P.getPosition
  return (Lex tok (Loc start end))

lexem :: [Char] -> Lexer (Lex Tok)
lexem prespaces = P.choice
  [ if "\n" `T.isSuffixOf` (T.pack prespaces)
    then
      do
        pos <- P.getPosition
        return (Lex NonIndentedNewline (Loc pos pos))
    else
      P.unexpected "indented newline"
  , atomSpace IfTok "if"
  , atomSpace ThenTok "then"
  , atomSpace ElseTok "else"
  , atomSpace ClassTok "class"
  , atomSpace WhereTok "where"
  , atomSpace InstanceTok "instance"
  , atomSpace DataTok "data"
  , atomSpace ForallTok "forall"
  , atomSpace CaseTok "case"
  , atomSpace OfTok "of"
  , atom PeriodTok "."
  , atom BackslashTok "\\"
  , atom OpenParenTok "("
  , atom CloseParenTok ")"
  , atom ImplyTok "=>"
  , atom EqualsTok "="
  , atom BarTok "|"
  , atom ColonsTok "::"
  , atom CommaTok ","
  , atom RightArrowTok "->"
  , atomSpace LetTok "let"
  , atomSpace InTok "in"
  , do
      tok <- lexing OpTok (T.pack <$> P.choice
                            [ P.string "*"
                            , P.string "+"
                            , P.try (P.string ">=")
                            , P.try (P.string "<=")
                            , P.string ">"
                            , P.string "<"
                            , P.string "/"
                            ]) "operator (e.g. *, <, +, etc.)"
      when (null prespaces) (P.unexpected (show tok ++ ", there should be spaces before and after operators."))
      P.lookAhead spaces1 <?> ("space after " ++ show tok)
      return tok
  , lexing StringTok (do
                         _ <- P.string "\""
                         chars <- P.many (P.satisfy (\c -> c /= '"'))
                         when (any (== '\\') chars) (P.unexpected "\\ character, not allowed inside a string.")
                         _ <- P.string "\""
                         return (T.pack chars)
                     ) "string (e.g. \"hello\", \"1234\", etc.)"
  , lexingSpec CharTok (do
                           _ <- P.string "'"
                           chars <- P.many1 (P.satisfy (\c -> c /= '\n' && c /= '\'')) <?> "character e.g. 'a'"
                           when (length chars > 1) (P.unexpected $ concat [ "character: you wrote\n"
                                                                          , "'" ++ (ellipsis 5 chars) ++ "'\n"
                                                                          , "but only one character is allowed inside single quotes"
                                                                          ])
                           _ <- P.string "'"
                           return (head chars)
                       ) "character (e.g. 'a', 'z', '9', etc.)"
  , lexing CtorTok (do
                       c <- P.satisfy isUpper
                       vars <- P.many (P.satisfy (flip elem (['A'..'Z'] ++ ['a'..'z'])))
                       return (T.singleton c <> T.pack vars)
                   ) "constructor (e.g. 'Rocket', 'Just', etc.)"
  , lexing VarTok (do
                       h <- P.many1 $ P.satisfy (flip elem ("_" ++ ['a'..'z']))
                       t <- P.many $ P.satisfy $ flip elem ("_" ++ ['a'..'z'] ++ ['A'..'Z']++['0'..'9'])
                       return (T.pack h <> T.pack t)
                   ) "variable (e.g. 'rocket', 'elephant', 't2', etc.)"
  , numbers prespaces
  ]

numbers :: [a] -> Lexer (Lex Tok)
numbers prespaces = lxer <?> "number (e.g. 42, 3.141, etc.)"
  where
    lxer = do
      start <- P.getPosition
      neg <- Just <$> P.char '-' <|> return Nothing
      let
        operator :: Lexer (Lex Tok)
        operator = do
          end <- P.getPosition
          return (Lex (OpTok "-") (Loc start end))

        number :: (forall a . (Num a) => a -> a) -> Lexer (Lex Tok)
        number fn = do
          x <- P.many1 P.digit
          (do
              _ <- P.char '.'
              y <- P.many1 P.digit <?> ("decimal component, e.g. " ++ x ++ ".0")
              end <- P.getPosition
              return (Lex (FloatTok $ fn $ read (x ++ "." ++ y)) (Loc start end))) <|>
            (do
                end <- P.getPosition
                return (Lex (IntTok $ fn $ read x) (Loc start end))
            )

      case neg of
        Nothing -> number id
        Just _ -> do
          when (null prespaces) (P.unexpected (quotes "-" ++ ", there should be a space before it."))
          (number (* (-1)) <?> "number (e.g. 123)") <|>
            operator <* (P.space <?> ("space after operation " ++ quotes "-"))

lexingSpec :: (t1 -> t) -> Lexer t1 -> String -> Lexer (Lex t)
lexingSpec fn lxer desc = do
  start <- P.getPosition
  t1 <- lxer <?> desc
  end <- P.getPosition
  return (Lex (fn t1) (Loc start end))

lexing :: (Text -> t) -> Lexer Text -> String -> Lexer (Lex t)
lexing tokFn lxer desc = do
  start <- P.getPosition
  text <- lxer <?> desc
  mapM_
    (bailOnUnsupportedKeywords text)
    [ "as"
    , "case"
    , "ccall"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "forall"
    , "foreign"
    , "if"
    , "import"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "module"
    , "newtype"
    , "qualified"
    , "then"
    , "type"
    , "safe"
    , "unsafe"
    , "where"
    ]
  end <- P.getPosition
  return (Lex (tokFn text) (Loc start end))
    where
      supportedKeywords = [ "class"
                          , "case"
                          , "data"
                          , "else"
                          , "forall"
                          , "instance"
                          , "if"
                          , "then"
                          ]
      bailOnUnsupportedKeywords txt word =
        when (txt == word) (P.unexpected $ if word `elem` supportedKeywords
                                           then
                                             "the keyword " ++ quotes (T.unpack word) ++ " isn't in the right place or is incomplete. Try adding a space after it?"
                                           else
                                             quotes (T.unpack word) ++ " : that keyword isn't allowed" ++ ext)
        where
          ext = "but you could use this " ++ quotes (T.unpack word ++ "_") ++ " instead"

