module Data.BBCode.Types (
  ParseEff,
  ParseState,
  defaultParseState,
  ParseReader,
  defaultParseReader,
  Token (..),
  flattenTokens,
  BBCodeMap,
  BBCodeFn,
  TagName,
  Parameters,
  ErrorMsg,
  BBDoc (..),
  BBCode (..),
  BBSize (..),
  BBList (..),
  BBTable (..),
  BBColor (..),
  ImageSize (..),
  QuoteAuthor,
  LinkURL,
  LinkName,
  MediaURL,
  ImageHeight,
  ImageWidth,
  FontOpts (..),
  defaultFontOpts,
  LinkOpts (..),
  defaultLinkOpts,
  ImageOpts (..),
  defaultImageOpts,
  SizeOpts (..),
  defaultSizeOpts,
  ColorOpts (..),
  defaultColorOpts
) where



import Control.Monad.RWS (RWS)
import Data.BBCode.Misc  (intersperse)
import Data.Either       (Either)
import Data.Foldable     (foldl)
import Data.List         (List(..))
import Data.Map          as M
import Data.Maybe        (Maybe(..))
import Data.String       (toLower)
import Data.Tuple        (Tuple)
import Prelude           (Unit, class Show, show, class Eq, map, (<>), (==), (<<<), (&&), (+), (-), ($))



type ParseState = {
  accum  :: List BBCode,
  stack  :: List (Tuple (Maybe Parameters) TagName),
  saccum :: List (Tuple Int BBCode)
}

defaultParseState :: ParseState
defaultParseState = { accum: Nil, stack: Nil, saccum: Nil }



type ParseReader = {
  linkOpts  :: LinkOpts,
  imageOpts :: ImageOpts,
  trfm      :: M.Map String (BBCode -> BBCode)
}

defaultParseReader :: ParseReader
defaultParseReader = {
  linkOpts:  defaultLinkOpts,
  imageOpts: defaultImageOpts,
  trfm:      M.empty
}



type ParseEff = RWS ParseReader Unit ParseState



data Token
  = BBOpen   (Maybe Parameters) TagName
  | BBClosed TagName
  | BBStr    String

instance tokenShow :: Show Token where
  show (BBOpen Nothing s)  = "open("<>toLower s<>")"
  show (BBOpen (Just p) s) = "open("<>toLower s<>", "<>toLower p<>")"
  show (BBClosed s)        = "closed("<>toLower s<>")"
  show (BBStr s)           = "str("<>s<>")"

instance tokenEq :: Eq Token where
  eq (BBOpen p1 t1)   (BBOpen p2 t2) = p1 == p2 && t1 == t2
  eq (BBClosed t1) (BBClosed t2)     = t1 == t2
  eq (BBStr t1)    (BBStr t2)        = t1 == t2
  eq _             _                 = false

flattenTokens :: List Token -> String
flattenTokens = foldl (<>) "" <<< intersperse "," <<< map show



type BBCodeMap = M.Map TagName BBCodeFn

type BBCodeFn   = (Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode)
type TagName    = String
type Parameters = String
type ErrorMsg   = String



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
  | Quote      (Maybe QuoteAuthor) (List BBCode)
  | Link       (Maybe LinkName) LinkURL
  | List       BBList
  | OrdList    BBList
  | Table      BBTable
  | Pre        String
  | Code       (Maybe String) String
  | Move       (List BBCode)
  | Text       String
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

instance bbcodeShow :: Show BBCode where
  show (Bold t)          = "Bold("<>show t<>")"
  show (Italic t)        = "Italic("<>show t<>")"
  show (Underline t)     = "Underline("<>show t<>")"
  show (Strike t)        = "Strike("<>show t<>")"
  show (Font _ t)        = "Font("<>show t<>")"
  show (Size _ t)        = "Size("<>show t<>")"
  show (Color _ t)       = "Color("<>show t<>")"
  show (Center _)        = "Center"
  show (AlignLeft _)     = "Left"
  show (AlignRight _)    = "Right"
  show (Quote _ _)       = "Quote"
  show (Link _ _)        = "Link"
  show (List _)          = "List"
  show (OrdList _)       = "OrdList"
  show (Table _)         = "Table"
  show (Pre t)           = "Pre("<>show t<>")"
  show (Code lang t)     = "Code("<>show lang<>","<>show t<>")"
  show (Move t)          = "Move("<>show t<>")"
  show (Text t)          = "Text("<>t<>")"

  show (Image _ url)     = "Image"
  show (Youtube url)     = "Youtube("<>url<>")"
  show (Vimeo url)       = "Vimeo("<>url<>")"
  show (Facebook url)    = "Facebook("<>url<>")"
  show (Instagram url)   = "Instagram("<>url<>")"
  show (Streamable url)  = "Streamable("<>url<>")"
  show (Imgur url)       = "Imgur("<>url<>")"

  show HR                = "HR"
  show NL                = "NL"
  show None              = "None"

instance bbcodeEq :: Eq BBCode where
  eq (Bold t1)      (Bold t2)      = t1 == t2
  eq (Italic t1)    (Italic t2)    = t1 == t2
  eq (Underline t1) (Underline t2) = t1 == t2
  eq (Strike t1)    (Strike t2)    = t1 == t2
  eq (Font o1 t1)   (Font o2 t2)   = o1 == o2 && t1 == t2
  eq (Size s1 t1)   (Size s2 t2)   = s1 == s2 && t1 == t2
  eq (Color c1 t1)  (Color c2 t2)  = c1 == c2 && t1 == t2
  eq (Center t1)    (Center t2)    = t1 == t2
  eq (AlignLeft t1) (AlignRight t2)= t1 == t2
  eq (Quote a1 t1)  (Quote a2 t2)  = a1 == a2 && t1 == t2
  eq (Link n1 t1)   (Link n2 t2)   = n1 == n2 && t1 == t2
  eq (List t1)      (List t2)      = t1 == t2
  eq (OrdList t1)   (OrdList t2)   = t1 == t2
  eq (Table t1)     (Table t2)     = t1 == t2
  eq (Pre t1)       (Pre t2)       = t1 == t2
  eq (Code l1 t1)   (Code l2 t2)   = l1 == l2 && t1 == t2
  eq (Move t1)      (Move t2)      = t1 == t2
  eq (Text t1)      (Text t2)      = t1 == t2

  eq (Image opts1 url1 ) (Image opts2 url2)     = opts1 == opts2 && url1 == url2
  eq (Youtube url1) (Youtube url2)              = url1 == url2
  eq (Vimeo url1) (Vimeo url2)                  = url1 == url2
  eq (Facebook url1) (Facebook url2)            = url1 == url2
  eq (Instagram url1) (Instagram url2)          = url1 == url2
  eq (Streamable url1) (Streamable url2)        = url1 == url2
  eq (Imgur url1) (Imgur url2)                  = url1 == url2

  eq HR             HR             = true
  eq NL             NL             = true
  eq None           None           = true
  eq _              _              = false



data BBSize
  = SizePx Int
  | SizePt Int
  | SizeEm Int

instance bbsizeEq :: Eq BBSize where
  eq (SizePx t1)  (SizePx t2)  = t1 == t2
  eq (SizePt t1)  (SizePt t2)  = t1 == t2
  eq (SizeEm t1)  (SizeEm t2)  = t1 == t2
  eq _            _            = false



data BBList
  = ListItem BBCode

instance bblistEq :: Eq BBList where
  eq (ListItem t1) (ListItem t2) = t1 == t2



data BBTable
  = TableRow BBCode

instance bbtableEq :: Eq BBTable where
  eq (TableRow t1) (TableRow t2) = t1 == t2



data ImageSize
  = ImagePx Int
  | ImagePercent Number

instance imageSizeEq :: Eq ImageSize where
  eq (ImagePx t1)      (ImagePx t2)      = t1 == t2
  eq (ImagePercent t1) (ImagePercent t2) = t1 == t2
  eq _                 _                 = false



data BBColor
  = ColorName String
  | ColorHex  String

instance bbcolorEq :: Eq BBColor where
  eq (ColorName t1) (ColorName t2) = true
  eq (ColorHex t1)  (ColorHex t2)  = true
  eq _              _     = false



type QuoteAuthor = String
type LinkURL     = String
type LinkName    = String
type MediaURL    = String
type ImageHeight = ImageSize
type ImageWidth  = ImageSize



newtype LinkOpts = LinkOpts {
  linkName :: Maybe String
}

instance linkOptsEq :: Eq LinkOpts where
  eq (LinkOpts o1) (LinkOpts o2) = o1.linkName == o2.linkName

defaultLinkOpts :: LinkOpts
defaultLinkOpts = LinkOpts {
  linkName: Nothing
}

defaultSafeLinkOpts :: LinkOpts
defaultSafeLinkOpts = defaultLinkOpts

newtype ImageOpts = ImageOpts {
  imageHeight :: Maybe ImageHeight,
  imageWidth  :: Maybe ImageWidth
}

instance imageOptsEq :: Eq ImageOpts where
  eq (ImageOpts o1) (ImageOpts o2) = o1.imageHeight == o2.imageHeight && o1.imageWidth == o2.imageWidth

defaultImageOpts :: ImageOpts
defaultImageOpts = ImageOpts {
  imageHeight: Nothing,
  imageWidth:  Nothing
}

defaultSafeImageOpts :: ImageOpts
defaultSafeImageOpts = ImageOpts {
  imageHeight: Just $ ImagePx 300,
  imageWidth:  Just $ ImagePx 300
}

newtype FontOpts = FontOpts {
  fontFamily :: Maybe String,
  fontFaces  :: Array String
}

instance fontOptsEq :: Eq FontOpts where
  eq (FontOpts o1) (FontOpts o2) = o1.fontFamily == o2.fontFamily && o1.fontFaces == o2.fontFaces

defaultFontOpts :: FontOpts
defaultFontOpts = FontOpts {
  fontFamily: Nothing,
  fontFaces:  []
}

newtype SizeOpts = SizeOpts {
  sizeValue :: Maybe BBSize
}

instance sizeOptsEq :: Eq SizeOpts where
  eq (SizeOpts o1) (SizeOpts o2) = o1.sizeValue == o2.sizeValue

defaultSizeOpts :: SizeOpts
defaultSizeOpts = SizeOpts {
  sizeValue: Nothing
}

newtype ColorOpts = ColorOpts {
  colorValue :: Maybe BBColor
}

instance colorOptsEq :: Eq ColorOpts where
  eq (ColorOpts o1) (ColorOpts o2) = o1.colorValue == o2.colorValue

defaultColorOpts :: ColorOpts
defaultColorOpts = ColorOpts {
  colorValue: Nothing
}
