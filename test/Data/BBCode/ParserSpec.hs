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
        `shouldBe` (Right "BBOpen Nothing \"b\",BBOpen Nothing \"u\",BBStr \"hello\",BBClosed \"u\",BBClosed \"b\"")

      flattenTokens <$> parseTokens' "[b][u][/u][/b]"
        `shouldBe` (Right "BBOpen Nothing \"b\",BBOpen Nothing \"u\",BBClosed \"u\",BBClosed \"b\"")

      flattenTokens <$> parseTokens' "[b][u][/u]"
        `shouldBe` (Right "BBOpen Nothing \"b\",BBOpen Nothing \"u\",BBClosed \"u\"")

      flattenTokens <$> parseTokens' "[b]a[u]b[/u]c"
        `shouldBe` (Right "BBOpen Nothing \"b\",BBStr \"a\",BBOpen Nothing \"u\",BBStr \"b\",BBClosed \"u\",BBStr \"c\"")

      flattenTokens <$> parseTokens' "[url=someurl]name[/url]"
        `shouldBe` (Right "BBOpen (Just \"someurl\") \"url\",BBStr \"name\",BBClosed \"url\"")

      flattenTokens <$> parseTokens' "[quote author=adarqui]hello[/quote]"
        `shouldBe` (Right "BBOpen (Just \"author=adarqui\") \"quote\",BBStr \"hello\",BBClosed \"quote\"")

      flattenTokens <$> parseTokens' "[youtube]https://www.youtube.com/watch?v=video[/youtube]"
        `shouldBe` (Right "BBOpen Nothing \"youtube\",BBStr \"https://www.youtube.com/watch?v=video\",BBClosed \"youtube\"")



  describe "parseBBCode" $ do
    it "parses a string into bbcode tags" $ do

      parseBBCode "hello"
        `shouldBe` (Right $ Cons (Text "hello") Nil)

      parseBBCode "/:?"
        `shouldBe` (Right $ Cons (Text "/:?") Nil)

      parseBBCode "["
        `shouldBe` (Right $ Cons (Text "[") Nil)

      parseBBCode "]"
        `shouldBe` (Right $ Cons (Text "]") Nil)

      parseBBCode "[ b ]"
        `shouldBe` (Right $ Cons (Text "[ b ]") Nil)

      parseBBCode "[/ b ]"
        `shouldBe` (Right $ Cons (Text "[/ b ]") Nil)

      parseBBCode "[/b]hello"
        `shouldBe` (Left "b not pushed")

      parseBBCode "[b]hello"
        `shouldBe` (Left "b not closed")

      parseBBCode "[b]hello[/b]"
        `shouldBe` (Right $ Cons (Bold (Cons (Text "hello") Nil)) Nil)
