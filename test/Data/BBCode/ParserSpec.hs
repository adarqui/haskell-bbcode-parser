{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.ParserSpec (
  main,
  spec
) where



import           Data.BBCode.Internal
import           Data.BBCode.Parser
import           Data.BBCode.Types
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do



  describe "concatTokens" $ do
    it "concatenates subsequent BBStr tokens into one BBStr token" $ do

     concatTokens (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))
       `shouldBe` (Cons (BBStr "ping pong") Nil)

     concatTokens (Cons (BBStr "ping") (Cons (BBOpen Nothing "b") (Cons (BBStr "pong") Nil)))
       `shouldBe` (Cons (BBStr "ping") (Cons (BBOpen Nothing "b") (Cons (BBStr "pong") Nil)))



  describe "concatBBStr" $ do
    it "concatenates subsequent BBStr tokens into one BBStr token" $ do

     concatBBStr (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))
       `shouldBe` (BBStr "ping pong")
