{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.TypesSpec (
  main,
  spec
) where



import           Data.BBCode.Internal
import           Data.BBCode.Types
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do



  describe "flattenTokens" $ do
    it "flattens tokens into a string" $ do

      flattenTokens (Cons (BBStr "hi") Nil) `shouldBe` "BBStr \"hi\""

      flattenTokens (Cons (BBOpen Nothing "b") (Cons (BBStr "hi") (Cons (BBClosed "b") Nil)))
        `shouldBe` "BBOpen Nothing \"b\",BBStr \"hi\",BBClosed \"b\""
