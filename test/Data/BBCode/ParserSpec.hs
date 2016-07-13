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



  describe "parseTokens'" $ do
    it "parses a string into open, str, and closed tags" $ do


      parseTokens' "hello"
        `shouldBe` (Right $ Cons (BBStr "hello") Nil)

      parseTokens' "["
        `shouldBe` (Right $ Cons (BBStr "[") Nil)

      parseTokens' "]"
        `shouldBe` (Right $ Cons (BBStr "]") Nil)

      parseTokens' "[ b ]"
        `shouldBe` (Right $ Cons (BBStr "[ b ]") Nil)

      parseTokens' "[/ b ]"
        `shouldBe` (Right $ Cons (BBStr "[/ b ]") Nil)

      parseTokens' "[b]hello[/b]"
        `shouldBe` (Right $ Cons (BBOpen Nothing "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))

      parseTokens' "[B]hello[/B]"
        `shouldBe` (Right $ Cons (BBOpen Nothing "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))

      flattenTokens <$> parseTokens' "[b][u]hello[/u][/b]"
        `shouldBe` (Right "open(b),open(u),str(hello),closed(u),closed(b)")

      flattenTokens <$> parseTokens' "[b][u][/u][/b]"
        `shouldBe` (Right "open(b),open(u),closed(u),closed(b)")

      flattenTokens <$> parseTokens' "[b][u][/u]"
        `shouldBe` (Right "open(b),open(u),closed(u)")

      flattenTokens <$> parseTokens' "[b]a[u]b[/u]c"
        `shouldBe` (Right "open(b),str(a),open(u),str(b),closed(u),str(c)")

      flattenTokens <$> parseTokens' "[url=someurl]name[/url]"
        `shouldBe` (Right "open(url, someurl),str(name),closed(url)")

      flattenTokens <$> parseTokens' "[quote author=adarqui]hello[/quote]"
        `shouldBe` (Right "open(quote, author=adarqui),str(hello),closed(quote)")

      flattenTokens <$> parseTokens' "[youtube]https://www.youtube.com/watch?v=video[/youtube]"
        `shouldBe` (Right "open(youtube),str(https://www.youtube.com/watch?v=video),closed(youtube)")
