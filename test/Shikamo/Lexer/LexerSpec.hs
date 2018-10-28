module Shikamo.Lexer.LexerSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Shikamo.Lang.Expr
import           Shikamo.Lexer.Lexer
import           Shikamo.Lexer.Loc

import           Data.Text           (Text)
import           Text.Parsec.Error   (Message (..))
import           Text.Parsec.Pos     (newPos)

deriving instance Show Message

expects = Expect <$> [ "white space"
                     , "end of input"
                     , "space"
                     , "'if'"
                     , "'then'"
                     , "'else'"
                     ]

lexemizeT fp s = lexemize fp (s :: Text)

spec :: Spec
spec = do
  describe "Lexer" $ do
    describe "lexem" $ do
      it "unitTok" $ do
        l <- lexemizeT "<file>" "      \n"
        l `shouldBe` Right ([])

      it "FloatTok" $ do
        l <- lexemizeT "<file>" " -2.3"
        l `shouldBe` Right ([Lex {lexTok = FloatTok {floatTok = -2.3}, lexLoc = Loc {locStart = newPos "<file>" 1 2, locEnd = newPos "<file>" 1 6}}])

      it "IntTok" $ do
        l <- lexemizeT "<file>" " -2"
        l `shouldBe` Right ([Lex {lexTok = IntTok {intTok = -2}, lexLoc = Loc {locStart = newPos "<file>" 1 2, locEnd = newPos "<file>" 1 4}}])

      it "IfThenElseTok" $ do
        l <- lexemizeT "<file>" " if True then -2 else 1.03"
        l `shouldBe` Right ([ Lex { lexTok = IfTok, lexLoc = Loc {locStart = newPos "<file>" 1 2, locEnd = newPos "<file>" 1 4}}
                            , Lex { lexTok = CtorTok { ctorTok = "True" }, lexLoc = Loc {locStart = newPos "<file>" 1 5, locEnd = newPos "<file>" 1 9}}
                            , Lex { lexTok = ThenTok, lexLoc = Loc {locStart = newPos "<file>" 1 10, locEnd = newPos "<file>" 1 14}}
                            , Lex { lexTok = IntTok { intTok = -2 }, lexLoc = Loc {locStart = newPos "<file>" 1 15, locEnd = newPos "<file>" 1 17}}
                            , Lex { lexTok = ElseTok, lexLoc = Loc {locStart = newPos "<file>" 1 18, locEnd = newPos "<file>" 1 22}}
                            , Lex { lexTok = FloatTok { floatTok = 1.03 }, lexLoc = Loc {locStart = newPos "<file>" 1 23, locEnd = newPos "<file>" 1 27}}
                            ])

      describe "var fun decl" $ do
        describe "fun" $ do
          it "simple" $ do
            l <- lexemizeT "<file>" "fn = \\x -> x + 1"
            l `shouldBe` Right [ Lex {lexTok = VarTok {varTok = "fn"}, lexLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 3}}
                               , Lex {lexTok = EqualsTok, lexLoc = Loc {locStart = newPos "<file>" 1 4, locEnd = newPos "<file>" 1 5}}
                               , Lex {lexTok = BackslashTok, lexLoc = Loc {locStart = newPos "<file>" 1 6, locEnd = newPos "<file>" 1 7}}
                               , Lex {lexTok = VarTok {varTok = "x"}, lexLoc = Loc {locStart = newPos "<file>" 1 7, locEnd = newPos "<file>" 1 8}}
                               , Lex {lexTok = RightArrowTok, lexLoc = Loc {locStart = newPos "<file>" 1 9, locEnd = newPos "<file>" 1 11}}
                               , Lex {lexTok = VarTok {varTok = "x"}, lexLoc = Loc {locStart = newPos "<file>" 1 12, locEnd = newPos "<file>" 1 13}}
                               , Lex {lexTok = OpTok {opTok = "+"}, lexLoc = Loc {locStart = newPos "<file>" 1 14, locEnd = newPos "<file>" 1 15}}
                               , Lex {lexTok = IntTok {intTok = 1}, lexLoc = Loc {locStart = newPos "<file>" 1 16, locEnd = newPos "<file>" 1 17}}
                               ]

          it "explicit" $ do
            pendingWith "yet to be implemented"
        describe "var" $ do
          it "simple" $ do
            pendingWith "yet to be implemented"
          it "explicit" $ do
            pendingWith "yet to be implemented"

      describe "data type decl" $ do
        describe "unkinded" $ do
          it "simple" $ do
            pendingWith "yet to be implemented"
          it "complex" $ do
            pendingWith "yet to be implemented"
        describe "kinded" $ do
          it "simple" $ do
            l <- lexemizeT "<file>" "data Maybe (a :: Type) = Just a | Nothing"
            l `shouldBe` Right ([ Lex {lexTok = DataTok, lexLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 5}}
                                , Lex {lexTok = CtorTok {ctorTok = "Maybe"}, lexLoc = Loc {locStart = newPos "<file>" 1 6, locEnd = newPos "<file>" 1 11}}
                                , Lex {lexTok = OpenParenTok, lexLoc = Loc {locStart = newPos "<file>" 1 12, locEnd = newPos "<file>" 1 13}}
                                , Lex {lexTok = VarTok {varTok = "a"}, lexLoc = Loc {locStart = newPos "<file>" 1 13, locEnd = newPos "<file>" 1 14}}
                                , Lex {lexTok = ColonsTok, lexLoc = Loc {locStart = newPos "<file>" 1 15, locEnd = newPos "<file>" 1 17}}
                                , Lex {lexTok = CtorTok {ctorTok = "Type"}, lexLoc = Loc {locStart = newPos "<file>" 1 18, locEnd = newPos "<file>" 1 22}}
                                , Lex {lexTok = CloseParenTok, lexLoc = Loc {locStart = newPos "<file>" 1 22, locEnd = newPos "<file>" 1 23}}
                                , Lex {lexTok = EqualsTok, lexLoc = Loc {locStart = newPos "<file>" 1 24, locEnd = newPos "<file>" 1 25}}
                                , Lex {lexTok = CtorTok {ctorTok = "Just"}, lexLoc = Loc {locStart = newPos "<file>" 1 26, locEnd = newPos "<file>" 1 30}}
                                , Lex {lexTok = VarTok {varTok = "a"}, lexLoc = Loc {locStart = newPos "<file>" 1 31, locEnd = newPos "<file>" 1 32}}
                                , Lex {lexTok = BarTok, lexLoc = Loc {locStart = newPos "<file>" 1 33, locEnd = newPos "<file>" 1 34}}
                                , Lex {lexTok = CtorTok {ctorTok = "Nothing"}, lexLoc = Loc {locStart = newPos "<file>" 1 35, locEnd = newPos "<file>" 1 42}}
                                ])

          it "complex" $ do
            pendingWith "yet to be implemented"

      it "class" $ do
        l <- lexemizeT "<file>" "class Klass a b c where\n klassMethod :: a -> b -> c"
        l `shouldBe` Right ([ Lex {lexTok = ClassTok, lexLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 6}}
                            , Lex {lexTok = CtorTok {ctorTok = "Klass"}, lexLoc = Loc {locStart = newPos "<file>" 1 7, locEnd = newPos "<file>" 1 12}}
                            , Lex {lexTok = VarTok {varTok = "a"}, lexLoc = Loc {locStart = newPos "<file>" 1 13, locEnd = newPos "<file>" 1 14}}
                            , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 1 15, locEnd = newPos "<file>" 1 16}}
                            , Lex {lexTok = VarTok {varTok = "c"}, lexLoc = Loc {locStart = newPos "<file>" 1 17, locEnd = newPos "<file>" 1 18}}
                            , Lex {lexTok = WhereTok, lexLoc = Loc {locStart = newPos "<file>" 1 19, locEnd = newPos "<file>" 1 24}}
                            , Lex {lexTok = VarTok {varTok = "klassMethod"}, lexLoc = Loc {locStart = newPos "<file>" 2 2, locEnd = newPos "<file>" 2 13}}
                            , Lex {lexTok = ColonsTok, lexLoc = Loc {locStart = newPos "<file>" 2 14, locEnd = newPos "<file>" 2 16}}
                            , Lex {lexTok = VarTok {varTok = "a"}, lexLoc = Loc {locStart = newPos "<file>" 2 17, locEnd = newPos "<file>" 2 18}}
                            , Lex {lexTok = RightArrowTok, lexLoc = Loc {locStart = newPos "<file>" 2 19, locEnd = newPos "<file>" 2 21}}
                            , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 2 22, locEnd = newPos "<file>" 2 23}}
                            , Lex {lexTok = RightArrowTok, lexLoc = Loc {locStart = newPos "<file>" 2 24, locEnd = newPos "<file>" 2 26}}
                            , Lex {lexTok = VarTok {varTok = "c"}, lexLoc = Loc {locStart = newPos "<file>" 2 27, locEnd = newPos "<file>" 2 28}}
                            ])

      describe "instance" $ do
        it "ctor" $ do
          l <- lexemizeT "<file>" "instance Klass ByteString Text Char where\n klass = \\ b t -> charFrom(b, t)"
          l `shouldBe` Right ([ Lex {lexTok = InstanceTok, lexLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 9}}
                              , Lex {lexTok = CtorTok {ctorTok = "Klass"}, lexLoc = Loc {locStart = newPos "<file>" 1 10, locEnd = newPos "<file>" 1 15}}
                              , Lex {lexTok = CtorTok {ctorTok = "ByteString"}, lexLoc = Loc {locStart = newPos "<file>" 1 16, locEnd = newPos "<file>" 1 26}}
                              , Lex {lexTok = CtorTok {ctorTok = "Text"}, lexLoc = Loc {locStart = newPos "<file>" 1 27, locEnd = newPos "<file>" 1 31}}
                              , Lex {lexTok = CtorTok {ctorTok = "Char"}, lexLoc = Loc {locStart = newPos "<file>" 1 32, locEnd = newPos "<file>" 1 36}}
                              , Lex {lexTok = WhereTok, lexLoc = Loc {locStart = newPos "<file>" 1 37, locEnd = newPos "<file>" 1 42}}
                              , Lex {lexTok = VarTok {varTok = "klass"}, lexLoc = Loc {locStart = newPos "<file>" 2 2, locEnd = newPos "<file>" 2 7}}
                              , Lex {lexTok = EqualsTok, lexLoc = Loc {locStart = newPos "<file>" 2 8, locEnd = newPos "<file>" 2 9}}
                              , Lex {lexTok = BackslashTok, lexLoc = Loc {locStart = newPos "<file>" 2 10, locEnd = newPos "<file>" 2 11}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 2 12, locEnd = newPos "<file>" 2 13}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 2 14, locEnd = newPos "<file>" 2 15}}
                              , Lex {lexTok = RightArrowTok, lexLoc = Loc {locStart = newPos "<file>" 2 16, locEnd = newPos "<file>" 2 18}}
                              , Lex {lexTok = VarTok {varTok = "charFrom"}, lexLoc = Loc {locStart = newPos "<file>" 2 19, locEnd = newPos "<file>" 2 27}}
                              , Lex {lexTok = OpenParenTok, lexLoc = Loc {locStart = newPos "<file>" 2 27, locEnd = newPos "<file>" 2 28}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 2 28, locEnd = newPos "<file>" 2 29}}
                              , Lex {lexTok = CommaTok, lexLoc = Loc {locStart = newPos "<file>" 2 29, locEnd = newPos "<file>" 2 30}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 2 31, locEnd = newPos "<file>" 2 32}}
                              , Lex {lexTok = CloseParenTok, lexLoc = Loc {locStart = newPos "<file>" 2 32, locEnd = newPos "<file>" 2 33}}
                              ])

        it "var" $ do
          l <- lexemizeT "<file>" "(Predicate b t c) => instance Klass b t c where\n klass = \\ b t -> charFrom(b, t)"
          l `shouldBe` Right ([ Lex {lexTok = OpenParenTok, lexLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2}}
                              , Lex {lexTok = CtorTok {ctorTok = "Predicate"}, lexLoc = Loc {locStart = newPos "<file>" 1 2, locEnd = newPos "<file>" 1 11}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 1 12, locEnd = newPos "<file>" 1 13}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 1 14, locEnd = newPos "<file>" 1 15}}
                              , Lex {lexTok = VarTok {varTok = "c"}, lexLoc = Loc {locStart = newPos "<file>" 1 16, locEnd = newPos "<file>" 1 17}}
                              , Lex {lexTok = CloseParenTok, lexLoc = Loc {locStart = newPos "<file>" 1 17, locEnd = newPos "<file>" 1 18}}
                              , Lex {lexTok = ImplyTok, lexLoc = Loc {locStart = newPos "<file>" 1 19, locEnd = newPos "<file>" 1 21}}
                              , Lex {lexTok = InstanceTok, lexLoc = Loc {locStart = newPos "<file>" 1 22, locEnd = newPos "<file>" 1 30}}
                              , Lex {lexTok = CtorTok {ctorTok = "Klass"}, lexLoc = Loc {locStart = newPos "<file>" 1 31, locEnd = newPos "<file>" 1 36}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 1 37, locEnd = newPos "<file>" 1 38}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 1 39, locEnd = newPos "<file>" 1 40}}
                              , Lex {lexTok = VarTok {varTok = "c"}, lexLoc = Loc {locStart = newPos "<file>" 1 41, locEnd = newPos "<file>" 1 42}}
                              , Lex {lexTok = WhereTok, lexLoc = Loc {locStart = newPos "<file>" 1 43, locEnd = newPos "<file>" 1 48}}
                              , Lex {lexTok = VarTok {varTok = "klass"}, lexLoc = Loc {locStart = newPos "<file>" 2 2, locEnd = newPos "<file>" 2 7}}
                              , Lex {lexTok = EqualsTok, lexLoc = Loc {locStart = newPos "<file>" 2 8, locEnd = newPos "<file>" 2 9}}
                              , Lex {lexTok = BackslashTok, lexLoc = Loc {locStart = newPos "<file>" 2 10, locEnd = newPos "<file>" 2 11}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 2 12, locEnd = newPos "<file>" 2 13}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 2 14, locEnd = newPos "<file>" 2 15}}
                              , Lex {lexTok = RightArrowTok, lexLoc = Loc {locStart = newPos "<file>" 2 16, locEnd = newPos "<file>" 2 18}}
                              , Lex {lexTok = VarTok {varTok = "charFrom"}, lexLoc = Loc {locStart = newPos "<file>" 2 19, locEnd = newPos "<file>" 2 27}}
                              , Lex {lexTok = OpenParenTok, lexLoc = Loc {locStart = newPos "<file>" 2 27, locEnd = newPos "<file>" 2 28}}
                              , Lex {lexTok = VarTok {varTok = "b"}, lexLoc = Loc {locStart = newPos "<file>" 2 28, locEnd = newPos "<file>" 2 29}}
                              , Lex {lexTok = CommaTok, lexLoc = Loc {locStart = newPos "<file>" 2 29, locEnd = newPos "<file>" 2 30}}
                              , Lex {lexTok = VarTok {varTok = "t"}, lexLoc = Loc {locStart = newPos "<file>" 2 31, locEnd = newPos "<file>" 2 32}}
                              , Lex {lexTok = CloseParenTok, lexLoc = Loc {locStart = newPos "<file>" 2 32, locEnd = newPos "<file>" 2 33}}
                              ])
