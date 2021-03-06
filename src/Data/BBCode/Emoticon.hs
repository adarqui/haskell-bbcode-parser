{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.Emoticon (
    Emoticons
  , defaultEmoticons
  , defaultEmoticonsBimap
  , printEmoticons
  , printEmoticonsIO
) where



import           Data.Bimap   (Bimap)
import qualified Data.Bimap   as Bimap
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text



type Emoticons = (Bimap Text Text, Text) -- ^ A Bimap of emoticons and their text-meaning, and a base url



defaultEmoticonsBimap :: Bimap Text Text
defaultEmoticonsBimap = Bimap.fromList $
  [ ( ":)"                       , "smile")
  , ( ":("                       , "frown")
  , ( ">:("                      , "angry")
  , ( "^-^"                      , "azn")
  , ( ":D"                       , "cheesy")
  , ( "8)"                       , "cool")
  , ( ":'("                      , "cry")
  , ( ":-["                      , "embarrassed")
  , ( ">:D"                      , "evil")
  , ( ";D"                       , "grin")
  , ( ":-*"                      , "kiss")
  , ( ":-X"                      , "lipsrsealed")
  , ( "::)"                      , "rolleyes")
  , ( ":o"                       , "shocked")
  , ( ":P"                       , "tongue")
  , ( ":-\\"                     , "undecided")
  , ( ";)"                       , "wink")
  , ( ":headbang:"               , "headbang")
  , ( ":huh:"                    , "huh")
  , ( ":facepalm:"               , "facepalm")
  , ( ":jumping:"                , "jumping")
  , ( ":running:"                , "running")
  , ( ":sprinting:"              , "sprinting")
  , ( ":cycling:"                , "cycling")
  , ( ":squatting:"              , "squatting")
  , ( ":personal-record:"        , "personal-record")
  , ( ":almostascoolasnyancat:"  , "almostascoolasnyancat")
  , ( ":derp:"                   , "derp")
  , ( ":goodjobbro:"             , "goodjobbro")
  , ( ":lololol:"                , "lololol")
  , ( ":ninja:"                  , "ninja")
  , ( ":raging:"                 , "raging")
  , ( ":wowthatwasnutswtf:"      , "wowthatwasnutswtf")
  , ( ":rant:"                   , "rant")
  , ( ":pissed:"                 , "pissed")
  , ( ":strong:"                 , "strong")
  , ( ":highfive:"               , "highfive")
  , ( ":trollface:"              , "trollface")
  , ( ":trolldance:"             , "trolldance")
  , ( ":uhcomeon:"               , "uhcomeon")
  , ( ":welcome:"                , "welcome")
  , ( ":ffffffuuuuuu:"           , "ffffffuuuuuu")
  , ( ":gtfo:"                   , "gtfo")
  , ( ":motherofgod:"            , "motherofgod")
  , ( ":pokerface:"              , "pokerface")
  ]



defaultEmoticons :: Emoticons
defaultEmoticons = (defaultEmoticonsBimap, "/emoticons")



-- | Just for printing the codes so I can paste them into my forum
--
printEmoticons :: Emoticons -> Text
printEmoticons (emoticons_bimap, _) = Text.intercalate " " $ Bimap.keys emoticons_bimap



printEmoticonsIO :: Emoticons -> IO ()
printEmoticonsIO = Text.putStrLn . printEmoticons
