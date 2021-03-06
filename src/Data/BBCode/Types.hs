{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.Types (
  ParseEff,
  ParseState (..),
  defaultParseState,
  ParseReader (..),
  defaultParseReader,
  Token (..),
  flattenTokens,
  BBCodeMap,
  BBCodeFn,
  TagName,
  Parameters,
  ErrorMsg,
  BBDoc,
  BBCode (..),
  BBSize (..),
  BBList (..),
  BBTable (..),
  BBColor (..),
  ImageSize (..),
  QuoteAuthor,
  AvatarURL,
  LinkText,
  LinkName,
  MediaURL,
  ImageHeight,
  ImageWidth,
  FontOpts (..),
  defaultFontOpts,
  LinkOpts (..),
  defaultLinkOpts,
  defaultSafeLinkOpts,
  ImageOpts (..),
  defaultImageOpts,
  defaultSafeImageOpts,
  SizeOpts (..),
  defaultSizeOpts,
  ColorOpts (..),
  defaultColorOpts
) where



import           Control.Monad.RWS    (RWS)
import           Data.Either          (Either)
import           Data.Foldable        (foldl)
import qualified Data.List            as List (intersperse)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (Maybe (..))
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text (pack)
import           Prelude              (Bool(..), Double, Eq, Int, Show, map, show, ($))

import           Data.BBCode.Emoticon (Emoticons)
import           Data.BBCode.Internal (List, Tuple, Unit, (<<<))



data ParseState = ParseState {
  accum  :: List BBCode,
  stack  :: List (Tuple (Maybe Parameters) TagName),
  saccum :: List (Tuple Int BBCode)
}

defaultParseState :: ParseState
defaultParseState = ParseState {
  accum  = [],
  stack  = [],
  saccum = []
}



data ParseReader = ParseReader {
  linkOpts       :: LinkOpts,                        -- ^ link options
  imageOpts      :: ImageOpts,                       -- ^ image options
  trfm           :: Map.Map Text (BBCode -> BBCode), -- ^ transformations on a BBCode tag
  allowNotClosed :: Bool,                            -- ^ Allows unclosed tags etc. Turns them into Text.
  emoticons      :: Maybe Emoticons,                 -- ^ A Map of emoticons and their text-meaning, and a base url
  -- stuff for [quote] and perhaps other bbcodes
  authorResource :: Maybe (Text -> Text),            -- ^ Transform an "author" to a "resource" (url perhaps?)
  linkResource   :: Maybe (Text -> Text),            -- ^ Transform a "link" to a "resource" (url perhaps?)
  authorAvatar   :: Maybe (Text -> Text)             -- ^ Transform an "author" to an "avatar resource" (image url perhaps?)
}

defaultParseReader :: ParseReader
defaultParseReader = ParseReader {
  linkOpts       = defaultLinkOpts,
  imageOpts      = defaultImageOpts,
  trfm           = Map.empty,
  allowNotClosed = False,
  emoticons      = Nothing,
  authorResource = Nothing,
  linkResource   = Nothing,
  authorAvatar   = Nothing
}



type ParseEff = RWS ParseReader Unit ParseState



data Token
  = BBOpen   (Maybe Parameters) TagName
  | BBClosed TagName
  | BBStr    Text
  deriving (Show, Eq)



flattenTokens :: List Token -> Text
flattenTokens = foldl (<>) "" <<< List.intersperse "," <<< map (Text.pack <<< show)



type BBCodeMap = Map TagName BBCodeFn

type BBCodeFn   = (Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode)
type TagName    = Text
type Parameters = Text
type ErrorMsg   = Text



type BBDoc = List BBCode



data BBCode
  = Bold       (List BBCode)
  | Italic     (List BBCode)
  | Underline  (List BBCode)
  | Strike     (List BBCode)
  | Font       FontOpts (List BBCode)
  | Size       SizeOpts (List BBCode)
  | Color      ColorOpts (List BBCode)
  | Center     (List BBCode)
  | AlignLeft  (List BBCode)
  | AlignRight (List BBCode)
  | Quote      (Maybe QuoteAuthor) (Maybe AvatarURL) (Maybe LinkText) (Maybe DateText) (List BBCode)
  | Link       (Maybe LinkName) LinkText
  | List       BBList
  | OrdList    BBList
  | Table      BBTable
  | Pre        Text
  | Code       (Maybe Text) Text
  | Move       (List BBCode)
  | Emoticon   Text
  | Text       Text
  | Image      ImageOpts MediaURL
  | Youtube    MediaURL
  | Vimeo      MediaURL
  | Facebook   MediaURL
  | Instagram  MediaURL
  | Streamable MediaURL
  | Imgur      MediaURL
  | HR
  | NL
  | None
  deriving (Eq, Show)



data BBSize
  = SizePx Int
  | SizePt Int
  | SizeEm Int
  deriving (Eq, Show)



data BBList
  = ListItem BBCode
  deriving (Eq, Show)



data BBTable
  = TableRow BBCode
  deriving (Eq, Show)



data ImageSize
  = ImagePx Int
  | ImagePercent Double
  deriving (Eq, Show)



data BBColor
  = ColorName Text
  | ColorHex  Text
  deriving (Eq, Show)



type QuoteAuthor = Text
type AvatarURL   = Text
type LinkText    = Text
type LinkName    = Text
type DateText    = Text
type MediaURL    = Text
type ImageHeight = ImageSize
type ImageWidth  = ImageSize



data LinkOpts = LinkOpts {
  linkName :: Maybe Text
} deriving (Eq, Show)

defaultLinkOpts :: LinkOpts
defaultLinkOpts = LinkOpts {
  linkName = Nothing
}

defaultSafeLinkOpts :: LinkOpts
defaultSafeLinkOpts = defaultLinkOpts

data ImageOpts = ImageOpts {
  imageHeight :: Maybe ImageHeight,
  imageWidth  :: Maybe ImageWidth
} deriving (Eq, Show)

defaultImageOpts :: ImageOpts
defaultImageOpts = ImageOpts {
  imageHeight = Nothing,
  imageWidth  = Nothing
}

defaultSafeImageOpts :: ImageOpts
defaultSafeImageOpts = ImageOpts {
  imageHeight = Just $ ImagePx 300,
  imageWidth  =  Just $ ImagePx 300
}

data FontOpts = FontOpts {
  fontFamily :: Maybe Text,
  fontFaces  :: List Text
} deriving (Eq, Show)

defaultFontOpts :: FontOpts
defaultFontOpts = FontOpts {
  fontFamily = Nothing,
  fontFaces =  []
}

data SizeOpts = SizeOpts {
  sizeValue :: Maybe BBSize
} deriving (Eq, Show)

defaultSizeOpts :: SizeOpts
defaultSizeOpts = SizeOpts {
  sizeValue = Nothing
}

data ColorOpts = ColorOpts {
  colorValue :: Maybe BBColor
} deriving (Eq, Show)

defaultColorOpts :: ColorOpts
defaultColorOpts = ColorOpts {
  colorValue = Nothing
}
