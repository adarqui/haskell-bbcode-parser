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
  [ (":)"         , "smile")
  , (":("         , "frown")
  , (":ibjumping:", "ibjumping")
  , (":ibrunning:", "ibrunning")
  ]



defaultEmoticons :: Emoticons
defaultEmoticons = (defaultEmoticonsMap, "/emoticons")
