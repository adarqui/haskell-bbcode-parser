{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.ParserSpec (
  main,
  spec
) where



import           Data.BBCode.Internal
import           Data.BBCode.Parser
import           Data.BBCode.Types
import           Test.Hspec
import qualified Data.Text as Text



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

      parseBBCode "[b]https://adarq.org[/b]"
        `shouldBe` (Right $ Cons (Bold (Cons (Text "https://adarq.org") Nil)) Nil)

      parseBBCode "[u][b]hello[/b][/u]"
        `shouldBe` (Right $ Cons (Underline (Cons (Bold (Cons (Text "hello") Nil)) Nil)) Nil)

      parseBBCode "[i][u][b]hello[/b][/u][/i]"
        `shouldBe` (Right $ Cons (Italic (Cons (Underline (Cons (Bold (Cons (Text "hello") Nil)) Nil)) Nil)) Nil)

      parseBBCode "ping[b]hello[/b]pong"
        `shouldBe`
          (Right $
            Cons
              (Text "ping")
              (Cons
                (Bold (Cons (Text "hello") Nil))
              (Cons (Text "pong") Nil))
          )

      parseBBCode "zero[u]one[b]two[/b]three[/u]four"
        `shouldBe` (Right (Cons (Text("zero")) (Cons (Underline(Cons (Text("one")) (Cons (Bold(Cons (Text("two")) (Nil))) (Cons (Text("three")) (Nil))))) (Cons (Text("four")) (Nil)))))

      parseBBCode "[youtube]https://www.youtube.com/watch?v=video[/youtube]"
        `shouldBe` (Right (Cons (Youtube "https://www.youtube.com/watch?v=video") Nil))

      parseBBCode "[hr]"
        `shouldBe` (Right (Cons HR Nil))

      parseBBCode "\n"
        `shouldBe` (Right (Cons NL Nil))

      parseBBCode "\n\n"
        `shouldBe` (Right (Cons NL (Cons NL Nil)))

      parseBBCode "hi\n"
        `shouldBe` (Right (Cons (Text "hi") (Cons NL Nil)))

      parseBBCode "\nhi"
        `shouldBe` (Right (Cons NL (Cons (Text "hi") Nil)))

      parseBBCode "I am the [b]best[/b] man, I [b]deed[/b] it. [b]yup[/b]"
        `shouldBe`
          (Right (Cons (Text("I am the ")) (Cons (Bold(Cons (Text("best")) (Nil))) (Cons (Text(" man, I ")) (Cons (Bold(Cons (Text("deed")) (Nil))) (Cons (Text(" it. ")) (Cons (Bold(Cons (Text("yup")) (Nil))) (Nil))))))))

      parseBBCode "[url=someUrl]name[/url]"
        `shouldBe` (Right $ Cons (Link (Just "name") "someUrl") Nil)

      let
        bigString n      = Text.replicate n 'A')
        big_string_1024  = bigString 1024
        big_string_10024 = bigString 10024

      parseBBCode $ "[b]" <> big_string_1024 <> "[/b]"
        `shouldBe` (Right $ Cons (Bold (Cons (Text big_string_1024) Nil)) Nil)

      parseBBCode $ "[b]" <> big_string_10024 <> "[/b]"
        `shouldBe` (Right $ Cons (Bold (Cons (Text big_string_10024) Nil)) Nil)


--    Assert.equal
--      (Right $ Cons ..
--      $ parseBBCode "[quote author=adarqui]hello[/quote]"
