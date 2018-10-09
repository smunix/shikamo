module Shikamo.Parse.LexerSpec ( spec
                               ) where

import           Test.Hspec
import           Test.QuickCheck

import           Shikamo.Parse.Lexer
import qualified Text.Parsec         as Parsec

spec :: Spec
spec = do
  describe "lexer" $ do
    it "languageDef" $ do
      Parsec.parse (contents (identifier)) "languageDef" " aVariable " `shouldBe` Right "aVariable"
      Parsec.parse (contents (semiSep identifier)) "languageDef" " a; b; c " `shouldBe` Right ["a", "b", "c"]
      Parsec.parse (contents (semiSep identifier)) "languageDef" " a; b; c " `shouldBe` Right ["a", "b", "c"]
      Parsec.parse (contents (reservedOp "and")) "languageDef" "and" `shouldBe` Right ()
      Parsec.parse (contents (reservedOp "andP")) "languageDef" "andP" `shouldBe` Right ()
      Parsec.parse (contents (identifier)) "languageDef" " and " `shouldNotBe` Right "and"
      Parsec.parse (contents (operator)) "languageDef" " ==> " `shouldBe` Right "==>"
      Parsec.parse (contents (integer)) "languageDef" " 1234 " `shouldBe` Right 1234
      Parsec.parse (contents (parens (do
                                         i1 <- identifier
                                         o <- operator -- fails on reserved OPs
                                         i2 <- identifier
                                         return (i1 ++ o ++ i2)))) "languageDef" " (a+b) " `shouldBe` Right "a+b"
