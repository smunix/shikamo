module Shikamo.Parse.ParserSpec ( spec
                               ) where

import           Test.Hspec
import           Test.QuickCheck

import           Shikamo.Parse.Parser

spec :: Spec
spec = do
  describe "parser" $ do
    it "parse" $ do
      pendingWith "specify later"
