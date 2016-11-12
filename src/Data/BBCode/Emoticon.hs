{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.Emoticon (
    Emoticons
  , defaultEmoticons
  , defaultEmoticonsMap
) where



import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)



type Emoticons = (Map Text Text, Text) -- ^ A Map of emoticons and their text-meaning, and a base url



defaultEmoticonsMap :: Map Text Text
defaultEmoticonsMap = Map.fromList $
  [ ( ":)"                       , "smile")
  , ( ":("                       , "frown")
  , ( "O0"                       , "afro")
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
  , ( ":uhhhfacepalm:"           , "uhhhfacepalm")
  , ( ":jumping:"                , "jumping")
  , ( ":running:"                , "running")
  , ( ":sprinting:"              , "sprinting")
  , ( ":cycling:"                , "cycling")
  , ( ":squatting:"              , "squatting")
  , ( ":personal-record:"        , "personalrecord")
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
defaultEmoticons = (defaultEmoticonsMap, "/emoticons")
