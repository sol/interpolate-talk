{-# LANGUAGE CPP #-}
module Data.String.Interpolate.UtilSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import ToString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toString" $ do
    it "behaves like `show`" $ do
      property $ \n -> toString (n :: Int) `shouldBe` show n

    context "when used with String" $ do
      it "behaves like `id`" $ do
        property $ \s -> toString s `shouldBe` s
