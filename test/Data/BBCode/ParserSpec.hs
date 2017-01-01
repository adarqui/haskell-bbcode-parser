{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.ParserSpec (
  main,
  spec
) where



import           Data.BBCode.Emoticon
import           Data.BBCode.Parser
import           Data.BBCode.Types
import           Data.Monoid          ((<>))
import qualified Data.Map             as Map
import qualified Data.Text            as Text
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do



  describe "concatTokens" $ do
    it "concatenates subsequent BBStr tokens into one BBStr token" $ do

     concatTokens [BBStr "ping", BBStr " ", BBStr "pong"]
       `shouldBe` [BBStr "ping pong"]

     concatTokens [BBStr "ping", BBOpen Nothing "b", BBStr "pong"]
       `shouldBe` [BBStr "ping", BBOpen Nothing "b", BBStr "pong"]



  describe "concatBBStr" $ do
    it "concatenates subsequent BBStr tokens into one BBStr token" $ do

     concatBBStr [BBStr "ping", BBStr " ", BBStr "pong"]
       `shouldBe` (BBStr "ping pong")



  describe "splitParams" $ do
    it "should split params out into unparsed k=v pairs" $ do

      splitParams ""
        `shouldBe` Right []

      splitParams "k v"
        `shouldBe` Right ["k", "v"]

      splitParams "link=adarq.org"
        `shouldBe` Right ["link=adarq.org"]

      splitParams "author=adarqui link=adarq.org date=10101010"
        `shouldBe` Right ["author=adarqui","link=adarq.org","date=10101010"]


  describe "splitKVs" $ do
    it "should split params into a list of (k,v) tuples" $ do

      splitKVs (splitParams "k v p")
        `shouldBe` []

      splitKVs (splitParams "author=adarqui link=adarq.org date=10101010")
        `shouldBe` [("author","adarqui"),("link","adarq.org"),("date","10101010")]



  describe "buildParamMap" $ do
    it "should build a param map.." $ do

      buildParamMap ""
        `shouldBe` Map.empty

      buildParamMap "osdkskd = dk odk df =df =fff =df= d=f kdf odfk odfk ofd = k= "
        `shouldBe` Map.fromList [("d","f")]

      buildParamMap "k=v"
        `shouldBe` Map.fromList [("k","v")]

      buildParamMap "author=adarqui link=adarq.org date=10101010"
        `shouldBe` Map.fromList [("author","adarqui"), ("link","adarq.org"), ("date","10101010")]



  describe "parseTokens'" $ do
    it "parses a string into open, str, and closed tags" $ do


      parseTokens' "hello"
        `shouldBe` (Right $ [BBStr "hello"])

      parseTokens' "["
        `shouldBe` (Right $ [BBStr "["])

      parseTokens' "]"
        `shouldBe` (Right $ [BBStr "]"])

      parseTokens' "[ b ]"
        `shouldBe` (Right $ [BBStr "[ b ]"])

      parseTokens' "[/ b ]"
        `shouldBe` (Right $ [BBStr "[/ b ]"])

      parseTokens' "[b]hello[/b]"
        `shouldBe` (Right $ [BBOpen Nothing "b", BBStr "hello", BBClosed "b"])

      parseTokens' "[B]hello[/B]"
        `shouldBe` (Right $ [BBOpen Nothing "b", BBStr "hello", BBClosed "b"])

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
        `shouldBe` (Right $ [Text "hello"])

      parseBBCode "/:?"
        `shouldBe` (Right $ [Text "/:?"])

      parseBBCode "["
        `shouldBe` (Right $ [Text "["])

      parseBBCode "]"
        `shouldBe` (Right $ [Text "]"])

      parseBBCode "[ b ]"
        `shouldBe` (Right $ [Text "[ b ]"])

      parseBBCode "[/ b ]"
        `shouldBe` (Right $ [Text "[/ b ]"])

      parseBBCode "[/b]hello"
        `shouldBe` (Left "b not pushed")

      parseBBCode "90 [degree] angle"
        `shouldBe` (Right $ [Text "90 ", Text "[degree]", Text " angle"])

      parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "90 [degree] angle [b]yo[/b]"
        `shouldBe` (Right $ [Text "90 ", Text "[degree]", Text " angle ", Bold [Text "yo"]])

      parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "90 [degree] angle"
        `shouldBe` (Right $ [Text "90 ", Text "[degree]", Text " angle"])

      parseBBCode "[b]hello"
        `shouldBe` (Left "b not closed")

      -- TODO FIXME: should be [b]hello, not [b]
      parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "[b]hello"
        `shouldBe` (Right $ [Text "[b]"])

      parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "[tt]hello"
        `shouldBe` (Right $ [Text "[tt]", Text "hello"])

      parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "[tt]hello[/tt]"
        `shouldBe` (Right $ [Text "[tt]", Text "hello", Text "[/tt]"])

      -- TODO FIXME: this should work, need to re-write the "unsupported bbcode" portion of the parser
      -- parseBBCodeWith (defaultParseReader { allowNotClosed = True }) "[tt][sup]hello[/tt][/sup]bye"
      --   `shouldBe` (Right $ [Text "[tt]", Text "[sup]", Text "hello", Text "[/tt]", Text "[/sup]", Text "bye"])

      parseBBCode "[b]hello[/b]"
        `shouldBe` (Right $ [Bold [Text "hello"]])

      parseBBCode "[b]https://adarq.org[/b]"
        `shouldBe` (Right $ [Bold [Text "https://adarq.org"]])

      parseBBCode "[u][b]hello[/b][/u]"
        `shouldBe` (Right $ [Underline [Bold [Text "hello"]]])

      parseBBCode "[i][u][b]hello[/b][/u][/i]"
        `shouldBe` (Right $ [Italic [Underline [Bold [Text "hello"]]]])

      parseBBCode "ping[b]hello[/b]pong"
        `shouldBe` (Right $ [Text "ping", Bold [Text "hello"], Text "pong"])

      parseBBCode "zero[u]one[b]two[/b]three[/u]four"
        `shouldBe` (Right [Text "zero", Underline [Text "one", Bold [Text "two"], Text"three"], Text "four"])

      parseBBCode "[youtube]https://www.youtube.com/watch?v=video[/youtube]"
        `shouldBe` (Right [Youtube "https://www.youtube.com/watch?v=video"])

      parseBBCode "[hr]"
        `shouldBe` (Right [HR])

      parseBBCode "\n"
        `shouldBe` (Right [NL])

      parseBBCode "\n\n"
        `shouldBe` (Right [NL, NL])

      parseBBCode "hi\n"
        `shouldBe` (Right [Text "hi", NL])

      parseBBCode "\nhi"
        `shouldBe` (Right [NL, Text "hi"])

      parseBBCode "I am the [b]best[/b] man, I [b]deed[/b] it. [b]yup[/b]"
        `shouldBe`
          (Right [Text "I am the ", Bold [Text "best"], Text " man, I ", Bold [Text "deed"], Text " it. ", Bold [Text "yup" ]])

      parseBBCode "[url=someUrl]name[/url]"
        `shouldBe` (Right $ [Link (Just "name") "someUrl"])

      let
        bigString n      = Text.replicate n "A"
        big_string_1024  = bigString 1024
        big_string_10024 = bigString 10024

      parseBBCode ("[b]" <> big_string_1024 <> "[/b]")
        `shouldBe` (Right $ [Bold [Text big_string_1024]])

      parseBBCode ("[b]" <> big_string_10024 <> "[/b]")
        `shouldBe` (Right $ [Bold [Text big_string_10024]])

      parseBBCode "[quote]hello[/quote]"
        `shouldBe` (Right $ [Quote Nothing Nothing Nothing Nothing [Text "hello"]])

      parseBBCode "[quote meta=poop]hello[/quote meta=poop]"
        `shouldBe` (Right $ [Quote Nothing Nothing Nothing Nothing [Text "hello"]])

      parseBBCode "[quote author=author avatar=avatar link=link date=1306339931]hello[/quote]"
        `shouldBe` (Right $ [Quote (Just "author") (Just "avatar") (Just "link") (Just "1306339931") [Text "hello"]])

      parseBBCodeWith (defaultParseReader { emoticons = Just defaultEmoticons }) ":ninja:"
        `shouldBe` (Right $ [Emoticon "ninja"])

      parseBBCodeWith (defaultParseReader { emoticons = Just defaultEmoticons }) "hi :)..."
        `shouldBe` (Right $ [Text "hi ", Emoticon "smile", Text "..."])

      parseBBCodeWith (defaultParseReader { emoticons = Just defaultEmoticons }) "[b]:ninja:[/b]"
        `shouldBe` (Right $ [Bold [Emoticon "ninja"]])

      parseBBCodeWith (defaultParseReader { emoticons = Just defaultEmoticons }) "[quote]:ninja:[/quote]"
        `shouldBe` (Right $ [Quote Nothing Nothing Nothing Nothing [Emoticon "ninja"]])

      parseBBCode "[size 18px]hi[/size]"
        `shouldBe` (Right $ [Size (SizeOpts { sizeValue = Just $ SizePx 18 }) [Text "hi"]])

      parseBBCode "[size 18pt]hi[/size]"
        `shouldBe` (Right $ [Size (SizeOpts { sizeValue = Just $ SizePt 18 }) [Text "hi"]])

      parseBBCode "[size 18em]hi[/size]"
        `shouldBe` (Right $ [Size (SizeOpts { sizeValue = Just $ SizeEm 18 }) [Text "hi"]])

--    Assert.equal
--      (Right $ Cons ..
--      $ parseBBCode "[quote author=adarqui]hello[/quote]"


  describe "textAndEmoticons" $ do
    it "parses emoticons out of a list of bbcode tokens" $ do

      textAndEmoticons defaultEmoticonsBimap [Text ":ninja:"]
        `shouldBe` [Emoticon "ninja"]

      textAndEmoticons defaultEmoticonsBimap [Text "hi :)..."]
        `shouldBe` [Text "hi ", Emoticon "smile", Text "..."]

      textAndEmoticons defaultEmoticonsBimap [Text ":ninja: :) :jumping::running::squatting:"]
        `shouldBe` [Emoticon "ninja", Text " ", Emoticon "smile", Text " ", Emoticon "jumping", Emoticon "running", Emoticon "squatting"]

      textAndEmoticons defaultEmoticonsBimap [Text "1[b]:ninja:[/b]2"]
        `shouldBe` [Text "1[b]", Emoticon "ninja", Text "[/b]2"]
