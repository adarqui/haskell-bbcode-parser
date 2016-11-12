{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.Emoticon (
  defaultEmoticons
) where



import           Data.Text (Text)



defaultEmoticons :: [(Text, Text)]
defaultEmoticons =
  [ (":)"         , "smile")
  , (":("         , "frown")
  , (":ibjumping:", "ibjumping")
  , (":ibrunning:", "ibrunning")
  ]
