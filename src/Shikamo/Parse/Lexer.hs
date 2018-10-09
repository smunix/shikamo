module Shikamo.Parse.Lexer ( braces
                           , brackets
                           , contents
                           , float
                           , identifier
                           , integer
                           , operator
                           , parens
                           , reserved
                           , reservedOp
                           , semiSep
                           , Op(..)
                           , Operators(..)
                           , Parser(..)
                           , Str(..)
                           ) where

import qualified Data.Text.Lazy       as Text
import qualified Text.Parsec          as Parsec
import qualified Text.Parsec.Expr     as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.Token    as Parsec

type Str = Text.Text
type Parser m a    = Parsec.ParsecT Text.Text () m a
type Op m a        = Parsec.Operator Text.Text () m a
type Operators m a = Parsec.OperatorTable Text.Text () m a

reservedNames, reservedOps :: [ String ]
reservedNames = [ "let"
                , "in"
                , "if"
                , "then"
                , "else"
                , "or"
                , "and"
                ]
reservedOps = [ "->"
              , "=>"
              , "="
              , "\\"
              , "and"
              , "or"
              , "not"
              -- , "-"
              -- , "+"
              , "<="
              , "<"
              , ">="
              , ">"
              ]

-- | lexer : Shikamo's language definition
lexer :: (Monad m) => Parsec.GenTokenParser Str () m
lexer = Parsec.makeTokenParser langDef
  where
    langDef :: (Monad m) => Parsec.GenLanguageDef Str () m
    langDef = Parsec.LanguageDef { Parsec.commentStart    = "{-"
                                 , Parsec.commentEnd      = "-}"
                                 , Parsec.commentLine     = "--"
                                 , Parsec.nestedComments  = True
                                 , Parsec.identStart      = Parsec.letter
                                 , Parsec.identLetter     = Parsec.alphaNum Parsec.<|> Parsec.oneOf "_'"
                                 , Parsec.opStart         = Parsec.opLetter langDef
                                 , Parsec.opLetter        = Parsec.oneOf ":!#$%&*+./<=>?@\\^|-~"
                                 , Parsec.reservedNames   = reservedNames
                                 , Parsec.reservedOpNames = reservedOps
                                 , Parsec.caseSensitive   = True
                                 }

identifier :: (Monad m) => Parser m String
identifier = Parsec.identifier lexer

-- | parses a reserved name
reserved :: (Monad m) => String -> Parser m ()
reserved = Parsec.reserved lexer

reservedOp :: (Monad m) => String -> Parser m ()
reservedOp = Parsec.reservedOp lexer

operator :: (Monad m) => Parser m String
operator = Parsec.operator lexer

parens :: (Monad m) => Parser m a -> Parser m a
parens = Parsec.parens lexer

decimal :: (Monad m) => Parser m Integer
decimal = Parsec.decimal lexer

float :: (Monad m) => Parser m Double
float = Parsec.float lexer

integer :: (Monad m) => Parser m Integer
integer = Parsec.integer lexer

symbol :: (Monad m) => String -> Parser m String
symbol = Parsec.symbol lexer

brackets :: (Monad m) => Parser m a -> Parser m a
brackets = Parsec.brackets lexer

braces :: (Monad m) => Parser m a -> Parser m a
braces = Parsec.braces lexer

semi :: (Monad m) => Parser m String
semi = Parsec.semi lexer

semiSep :: (Monad m) => Parser m a -> Parser m [a]
semiSep = Parsec.semiSep lexer

contents :: (Monad m) =>  Parser m a -> Parser m a
contents p = do
  Parsec.whiteSpace lexer
  r <- p
  Parsec.eof
  return r
