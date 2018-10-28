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
    describe "data declaration" $ do
      it "simple" $ do
        lxs <- lexemizeT "<file>" "data Maybe (a :: Type) = Just a | Nothing"
        case lxs of
          Right lxs' -> do
            ast <- parseTest dataDecl "<file>" lxs'
            ast `shouldBe` Right (Located { locatedTok = DataType { dtName = Id {idString = "Maybe"}
                                                                  , dtVars = [TypeVar { tvId = Id {idString = "a"}
                                                                                      , tvKind = StarKind}
                                                                             ]
                                                                  , dtCtors = [ DataCtor { dtcName = Id {idString = "Just"}
                                                                                         , dtcFields = [UnkindedTpVar (Id {idString = "a"})]}
                                                                              , DataCtor { dtcName = Id {idString = "Nothing"}
                                                                                         , dtcFields = []}]
                                                                 }
                                          , locatedLoc = Loc {locStart = newPos "<file>" 1 1, locEnd = newPos "<file>" 1 5}
                                          })

