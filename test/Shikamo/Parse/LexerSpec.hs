module Shikamo.Parse.LexerSpec ( spec
                               ) where

import           Test.Hspec
import           Test.QuickCheck

import           Shikamo.Parse.Lexer

import qualified Data.Text.Lazy      as Text
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Expr    as Parsec
import qualified Text.Parsec.Token   as Parsec

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
                ]
reservedOps = [ "+"
              , "*"
              , "/"
              , "-"
              , "->"
              , "=>"
              , "="
              , "\\"
              , "and"
              , "or"
              , "not"
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
                                 , Parsec.opStart         = Parsec.oneOf ":!#$%&*+./<=>?@\\^|-~"
                                 , Parsec.opLetter        = Parsec.oneOf ":!#$%&*+./<=>?@\\^|-~"
                                 , Parsec.reservedNames   = reservedNames
                                 , Parsec.reservedOpNames = reservedOps
                                 , Parsec.caseSensitive   = True
                                 }

spec :: Spec
spec = do
  describe "lex" $ do
    it "utils" $ do
      pending
