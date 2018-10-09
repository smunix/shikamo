module Shikamo.Parse.ParserSpec ( spec
                                ) where

import           Test.Hspec
import           Test.QuickCheck

import           Shikamo.Lang.Expr
import           Shikamo.Parse.Lexer
import           Shikamo.Parse.Parser

import qualified Text.Parsec          as Parsec

spec :: Spec
spec = do
  describe "parser" $ do
    it "Var" $ do
      Parsec.parse (contents (varExpr)) "parser" " a " `shouldBe` Right (Var "a")
      Parsec.parse (contents (semiSep varExpr)) "parser" " aVar; bVar " `shouldBe` Right (Var <$> ["aVar", "bVar"])
    it "Arith" $ do
      Parsec.parse (contents (termExpr)) "parser" " a+b " `shouldBe` Right (BinOp Add (Var "a") (Var "b"))
      Parsec.parse (contents (parens (termExpr))) "parser" " (a+b) " `shouldBe` Right (BinOp Add (Var "a") (Var "b"))
      Parsec.parse (contents (termExpr)) "parser" " a+b+c " `shouldBe` Right (BinOp Add (BinOp Add (Var "a") (Var "b")) (Var "c"))
      Parsec.parse (contents (termExpr)) "parser" " (a+b)+c " `shouldBe` Right (BinOp Add (BinOp Add (Var "a") (Var "b")) (Var "c"))
      Parsec.parse (contents (termExpr)) "parser" " a+(b+c) " `shouldBe` Right (BinOp Add (Var "a") (BinOp Add (Var "b") (Var "c")))
      Parsec.parse (contents (termExpr)) "parser" " a-b*c " `shouldBe` Right (BinOp Sub (Var "a") (BinOp Mul (Var "b") (Var "c")))
      Parsec.parse (contents (termExpr)) "parser" " -a-b*c " `shouldBe` Right (BinOp Sub (UnaOp Neg (Var "a")) (BinOp Mul (Var "b") (Var "c")))
      Parsec.parse (contents (termExpr)) "parser" " -(a-b)*c " `shouldBe` Right (BinOp Mul (UnaOp Neg (BinOp Sub (Var "a") (Var "b"))) (Var "c"))

    it "Comp/Booleans" $ do
      Parsec.parse (contents (termExpr)) "parser" " a and b " `shouldBe` Right (BinOp And (Var "a") (Var "b"))
      Parsec.parse (contents (termExpr)) "parser" " a or b " `shouldBe` Right (BinOp Or (Var "a") (Var "b"))
      Parsec.parse (contents (parens (termExpr))) "parser" " (a and b) " `shouldBe` Right (BinOp And (Var "a") (Var "b"))
      Parsec.parse (contents (parens (termExpr))) "parser" " (a or b) " `shouldBe` Right (BinOp Or (Var "a") (Var "b"))
      Parsec.parse (contents (termExpr)) "parser" " a <= b > c " `shouldBe` Right (BinOp Gt (BinOp Lte (Var "a") (Var "b")) (Var "c"))
      Parsec.parse (contents (termExpr)) "parser" " a <= (b > c) " `shouldBe` Right (BinOp Lte (Var "a") (BinOp Gt (Var "b") (Var "c")))
      Parsec.parse (contents (termExpr)) "parser" " a>b and c < d or e >= f " `shouldBe` Right (BinOp Or (BinOp And (BinOp Gt (Var "a") (Var "b")) (BinOp Lt (Var "c") (Var "d"))) (BinOp Gte (Var "e") (Var "f")))

    it "Applications (untyped)" $ do
      Parsec.parse (contents (appExpr)) "parser" "a b c d" `shouldBe` Right (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d"))
      Parsec.parse (contents (appExpr)) "parser" "a (b+c) d < e f and g" `shouldBe` Right (App (App (App (Var "a") (BinOp Add (Var "b") (Var "c"))) (BinOp Lt (Var "d") (Var "e"))) (BinOp And (Var "f") (Var "g")))

    it "Lambda abstraction (untyped)" $ do
      Parsec.parse (contents (lamExpr)) "parser" "\\ x . x" `shouldBe` Right (Lam "x" (Var "x"))
      Parsec.parse (contents (lamExpr)) "parser" "\\ x y . x y" `shouldBe` Right (Lam "x" (Lam "y" (App (Var "x") (Var "y"))))

