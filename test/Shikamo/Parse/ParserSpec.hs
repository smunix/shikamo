module Shikamo.Parse.ParserSpec ( spec
                               ) where

import           Shikamo.Lang.Expr
import           Shikamo.Lexer.Lexer
import           Shikamo.Lexer.Loc
import           Shikamo.Parse.Parser

import           Data.Text            (Text)
import           Text.Parsec.Error    (Message (..))
import           Text.Parsec.Pos      (newPos)

import           Test.Hspec
import           Test.QuickCheck

deriving instance Show Message
lexemizeT fp s = lexemize fp (s :: Text)

spec :: Spec
spec = do
  describe "parser" $ do
    describe "var/function declariation" $ do
      it "addition function" $ do
        lxs <- lexemizeT "<file>" "f :: Int -> Int -> Int\nf = \\x y -> x + y"
        case lxs of
          Right lxs' -> do
            ast <- parseTest varFnDeclExpl "<file>" lxs'
            ast `shouldBe` Right (BindDecl
                                   (Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2})
                                   (ExplBinding
                                     (ExplTypedBinding { etbLabel = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2}
                                                       , etbId = (Id {idString = "f"},Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2})
                                                       , etbScheme = Forall [] (Qualified { qualPredicates = []
                                                                                          , qualType = TyUnkindApp
                                                                                                       (TyUnkindApp (TyUnkindCtor (Id {idString = "(->)"})) (TyUnkindCtor (Id {idString = "Int"})))
                                                                                                       (TyUnkindApp
                                                                                                         (TyUnkindApp (TyUnkindCtor (Id {idString = "(->)"})) (TyUnkindCtor (Id {idString = "Int"})))
                                                                                                         (TyUnkindCtor (Id {idString = "Int"})))
                                                                                          })
                                                       , etbAlts = [ Alt { altLabel = Loc {locStart = newPos "<file>" 2 5, locEnd = newPos "<file>" 2 6}
                                                                         , altPatterns = [ VarPat
                                                                                           (Loc {locStart = newPos "<file>" 2 6, locEnd = newPos "<file>" 2 7})
                                                                                           (Id {idString = "x"})
                                                                                         , VarPat
                                                                                           (Loc {locStart = newPos "<file>" 2 8, locEnd = newPos "<file>" 2 9})
                                                                                           (Id {idString = "y"})
                                                                                         ]
                                                                         , altExprs = InfixExpr
                                                                                      (Loc { locStart = newPos "<file>" 2 13, locEnd = newPos "<file>" 2 14})
                                                                                      (VarExpr
                                                                                        (Loc {locStart = newPos "<file>" 2 13, locEnd = newPos "<file>" 2 14})
                                                                                        (Id {idString = "x"}))
                                                                                      ( "+"
                                                                                      , VarExpr
                                                                                        (Loc {locStart = newPos "<file>" 2 15, locEnd = newPos "<file>" 2 16})
                                                                                        (Id {idString = "+"})
                                                                                      )
                                                                                      (VarExpr (Loc {locStart = newPos "<file>" 2 17, locEnd = newPos "<file>" 2 18}) (Id {idString = "y"}))
                                                                         }
                                                                   ]
                                                       })))

      it "function" $ do
        lxs <- lexemizeT "<file>" "f :: Int -> Int\nf = \\x -> x"
        case lxs of
          Right lxs' -> do
            ast <- parseTest varFnDeclExpl "<file>" lxs'
            ast `shouldBe` Right (BindDecl
                                   (Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2})
                                   (ExplBinding (ExplTypedBinding { etbLabel = Loc { locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2 }
                                                                  , etbId = (Id {idString = "f"},Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 2})
                                                                  , etbScheme = Forall [] (Qualified { qualPredicates = []
                                                                                                     , qualType = TyUnkindApp
                                                                                                                  (TyUnkindApp
                                                                                                                    (TyUnkindCtor (Id {idString = "(->)"}))
                                                                                                                    (TyUnkindCtor (Id {idString = "Int"})))
                                                                                                                  (TyUnkindCtor (Id {idString = "Int"}))})
                                                                  , etbAlts = [ Alt { altLabel = Loc {locStart = newPos "<file>" 2 5, locEnd = newPos "<file>" 2 6}
                                                                                    , altPatterns = [ VarPat
                                                                                                      (Loc {locStart = newPos "<file>" 2 6, locEnd = newPos "<file>" 2 7})
                                                                                                      (Id {idString = "x"})]
                                                                                    , altExprs = VarExpr
                                                                                                 (Loc {locStart = newPos "<file>" 2 11, locEnd = newPos "<file>" 2 12})
                                                                                                 (Id {idString = "x"})}
                                                                              ]
                                                                  })))

    describe "expr declaration" $ do
      it "lambda" $ do
        lxs <- lexemizeT "<file>" "\\x -> x"
        case lxs of
          Right lxs' -> do
            ast <- parseTest expr "<file>" lxs'
            ast `shouldBe` Right (LambExpr
                                   (Loc { locStart = newPos "<file>" 1 1
                                        , locEnd = newPos "<file>" 1 2})
                                   (Alt { altLabel = Loc { locStart = newPos "<file>" 1 1
                                                         , locEnd = newPos "<file>" 1 2
                                                         }
                                        , altPatterns = [ VarPat
                                                          (Loc { locStart = newPos "<file>" 1 2
                                                               , locEnd = newPos "<file>" 1 3
                                                               })
                                                          (Id {idString = "x"})
                                                        ]
                                        , altExprs = VarExpr
                                                     (Loc { locStart = newPos "<file>" 1 7
                                                          , locEnd = newPos "<file>" 1 8
                                                          })
                                                     (Id {idString = "x"})}))

    describe "data declaration" $ do
      it "dataDecl => data Maybe (a :: Type) = Just a | Nothing" $ do
        lxs <- lexemizeT "<file>" "data Maybe (a :: Type) = Just a | Nothing"
        case lxs of
          Right lxs' -> do
            ast <- parseTest dataDecl "<file>" lxs'
            ast `shouldBe` Right (Located { locatedTok = DataType { dtName = Id {idString = "Maybe"}
                                                                  , dtVars = [TypeVar { tvId = Id {idString = "a"}
                                                                                      , tvKind = StarKind}
                                                                             ]
                                                                  , dtCtors = [ DataCtor { dtcName = Id {idString = "Just"}
                                                                                         , dtcFields = [TyUnkindVar (Id {idString = "a"})]}
                                                                              , DataCtor { dtcName = Id {idString = "Nothing"}
                                                                                         , dtcFields = []}]
                                                                 }
                                          , locatedLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 5}
                                          })

      it "dataDecl => data Maybe a = Just (Variant Bool String a) | Nothing" $ do
        lxs <- lexemizeT "<file>" "data Maybe a = Just (Variant Bool String a) | Nothing"
        case lxs of
          Right lxs' -> do
            ast <- parseTest dataDecl "<file>" lxs'
            ast `shouldBe` Right (Located { locatedTok = DataType { dtName = Id {idString = "Maybe"}
                                                                  , dtVars = [TypeVar { tvId = Id {idString = "a"}
                                                                                      , tvKind = StarKind}]
                                                                  , dtCtors = [ DataCtor { dtcName = Id {idString = "Just"}
                                                                                         , dtcFields = [ TyUnkindApp (TyUnkindApp (TyUnkindApp (TyUnkindCtor (Id {idString = "Variant"})) (TyUnkindCtor (Id {idString = "Bool"}))) (TyUnkindCtor (Id {idString = "String"}))) (TyUnkindVar (Id {idString = "a"}))
                                                                                                       ]
                                                                                         }
                                                                              , DataCtor {dtcName = Id {idString = "Nothing"}, dtcFields = []}]
                                                                  }
                                          , locatedLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 5}
                                          })

      it "dataDecl => data Maybe a = Show a => Just (Variant Bool String a) | Nothing" $ do
        -- lxs <- lexemizeT "<file>" "data Maybe a = Show a => Just (Variant Bool String a) | Nothing"
        -- case lxs of
        --   Right lxs' -> do
        --     parseTest dataDecl "<file>" lxs'
        -- ast `shouldBe` Left {}
        pendingWith "Constraints are not supported on Data contructors"
