{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.Emoticon (
    Emoticons
  , defaultEmoticons
  , defaultEmoticonsBimap
) where



import           Data.Bimap  (Bimap)
import qualified Data.Bimap  as Bimap
import           Data.Text (Text)



type Emoticons = (Bimap Text Text, Text) -- ^ A Bimap of emoticons and their text-meaning, and a base url



defaultEmoticonsBimap :: Bimap Text Text
defaultEmoticonsBimap = Bimap.fromList $
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
  , ( ":ibjumping:"              , "jumping")
  , ( ":ibrunning:"              , "running")
  , ( ":ibsprinting:"            , "sprinting")
  , ( ":ibcycling:"              , "cycling")
  , ( ":ibsquatting:"            , "squatting")
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
defaultEmoticons = (defaultEmoticonsBimap, "/emoticons")
