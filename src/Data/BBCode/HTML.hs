module Data.BBCode.HTML (
  runBBCodeToHTML,
  runBBCodeToHTMLWith,
  bbcodeToHTML
) where



import Control.Monad.RWS               (evalRWS)
import Control.Monad.Reader            (ask)
import Data.Array                      as A
import Data.NonEmpty                   as NonEmpty
import Data.Int                        (toNumber)
import Data.List                       as L
import Data.List                       (List)
import Data.Maybe                      (Maybe(..))
import Data.Map                        as M
import Data.StrMap                     as StrM
import Data.Tuple                      (Tuple(..), fst)

import Color                           (Color, black, fromHexString) as Color
import Color.Scheme.HTML               (aqua, teal, blue, navy, yellow, olive, lime, green
                                       ,fuchsia, purple, red, maroon, gray, silver)
                                       as Color
import Halogen                         (HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed        (style) as CSS
import CSS.Font                        (GenericFontFamily(GenericFontFamily), color, fontFamily, sansSerif, fontSize) as CSS
import CSS.Size                        (pt, px) as CSS
import CSS.String                      (fromString) as CSS
import CSS.Text                        (underline, textDecoration) as CSS
import CSS.TextAlign                   (rightTextAlign, textAlign, leftTextAlign) as CSS
import Prelude                         (Unit, bind, pure, ($), (<>), (<$>))

import Data.BBCode.Types               (ParseReader, ParseEff, MediaURL, BBCode(..), BBColor(..), BBSize(..)
                                       ,ColorOpts(..), FontOpts(..), ImageOpts(..), ImageSize(..), SizeOpts(..)
                                       ,defaultParseState, defaultParseReader)



runBBCodeToHTML :: forall p i. List BBCode -> Array (HTML p (i Unit))
runBBCodeToHTML = runBBCodeToHTMLWith defaultParseReader



runBBCodeToHTMLWith :: forall p i. ParseReader -> List BBCode -> Array (HTML p (i Unit))
runBBCodeToHTMLWith parse_reader codes =
  fst $ evalRWS (bbcodeToHTML codes) parse_reader defaultParseState



-- bbcodeToHTML :: List BBCode -> HTML _ _
bbcodeToHTML :: forall p i. List BBCode -> ParseEff (Array (HTML p (i Unit)))
bbcodeToHTML codes = go [] codes
  where
  go acc xs =
    case L.uncons xs of
      Nothing                 -> pure $ A.reverse acc
      Just {head: h, tail: t} -> do
        html <- codeToHTML h
        go (html `A.cons` acc) t


-- codeToHTML :: 
codeToHTML :: forall p i. BBCode -> ParseEff (HTML p (i Unit))
codeToHTML tag = do
  case tag of
       Bold xs              -> H.strong_ <$> bbcodeToHTML xs
       Italic xs            -> H.em_ <$> bbcodeToHTML xs
       Underline xs         -> H.span [CSS.style $ CSS.textDecoration CSS.underline] <$> bbcodeToHTML xs
       Strike xs            -> H.del_ <$> bbcodeToHTML xs
       Font opts xs         -> runFont opts xs
       Size opts xs         -> runSize opts xs
       Color opts xs        -> runColor opts xs
       Center xs            -> H.center_ <$> bbcodeToHTML xs
       AlignLeft xs         -> H.p [CSS.style $ CSS.textAlign CSS.leftTextAlign] <$> bbcodeToHTML xs
       AlignRight xs        -> H.p [CSS.style $ CSS.textAlign CSS.rightTextAlign] <$> bbcodeToHTML xs
       Quote author xs      -> H.blockquote_ <$> bbcodeToHTML xs
       Link (Just name) url -> pure $ H.a [P.href url, P.target "_blank"] [H.text name]
       Link Nothing url     -> pure $ H.a [P.href url, P.target "_blank"] [H.text url]
--       List list            -- <ul><li>Red <li>Blue <li>Yellow</ul>
--       OrdList list         -- <ol style="list-style-type: decimal"><li>Red <li>Blue <li>Yellow</ol>
--       Table table
       Pre text             -> pure $ H.pre_ [H.text text]
       Code _ code          -> pure $ H.pre_ [H.text code]
       Move xs              -> pure $ H.text "[move] is deprecated"
       Text text            -> pure $ H.text text
       Image opts url       -> runImage opts url
       Youtube url          -> pure $ H.iframe [P.src url]
--        Vimeo url
--        Facebook url
--        Instagram url
--        Streamable url
--        Imgur url
       HR                  -> pure $ H.hr_
       NL                  -> pure $ H.br_
       _                   -> pure $ H.p_ [H.text "unknown"]



--
-- TODO FIXME: need to cleanup these redundant map lookups + trfm
--


runSize :: forall p i. SizeOpts -> List BBCode -> ParseEff (HTML p (i Unit))
runSize opts xs = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "size" r.trfm of
              Nothing   -> Size opts xs
              Just trfm -> trfm (Size opts xs))
  go code
  where
  go (Size (SizeOpts opts') xs) = do
    html <- bbcodeToHTML xs
    let size = (case opts'.sizeValue of
               Just (SizePx n)  -> CSS.px $ toNumber n
               Just (SizePt n)  -> CSS.pt $ toNumber n
               Just (SizeEm n)  -> CSS.pt $ toNumber n
               _                -> CSS.pt $ toNumber 12) -- TODO FIXME: default
    pure $ H.span [CSS.style $ CSS.fontSize size] html
  go _ = pure $ H.div_ []

--  H.span [CSS.style $ CSS.fontSize $ CSS.px $ toNumber px] <$> bbcodeToHTML xs



runFont :: forall p i. FontOpts -> List BBCode -> ParseEff (HTML p (i Unit))
runFont opts xs = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "font" r.trfm of
              Nothing   -> Font opts xs
              Just trfm -> trfm (Font opts xs))
  go code
  where
  go (Font (FontOpts opts') xs) = do
    html <- bbcodeToHTML xs
    let fam' = (case opts'.fontFamily of
              Nothing  -> CSS.sansSerif
              Just fam -> CSS.GenericFontFamily $ CSS.fromString fam)
    pure $ H.span [CSS.style $ CSS.fontFamily opts'.fontFaces (NonEmpty.singleton fam')] html
  go _ = pure $ H.div_ []




-- MAJO TODO FIXME: this needs to be in purescript-colors .. as well as maps for other schemes
-- however.. had major problems building purescript-colors release=0.4.4
-- odd stuff, 5 AM .. just going to implement this here for now
-- but i'd love to get this into purescript-colors
-- gn.
--
htmlSchemeMap :: StrM.StrMap Color.Color
htmlSchemeMap =
  StrM.fromFoldable [
    Tuple "silver"  Color.silver
  , Tuple "gray"    Color.gray
  , Tuple "maroon"  Color.maroon
  , Tuple "red"     Color.red
  , Tuple "purple"  Color.purple
  , Tuple "fuchsia" Color.fuchsia
  , Tuple "green"   Color.green
  , Tuple "lime"    Color.lime
  , Tuple "olive"   Color.olive
  , Tuple "yellow"  Color.yellow
  , Tuple "navy"    Color.navy
  , Tuple "blue"    Color.blue
  , Tuple "teal"    Color.teal
  , Tuple "aqua"    Color.aqua
  ]



runColor :: forall p i. ColorOpts -> List BBCode -> ParseEff (HTML p (i Unit))
runColor opts xs = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "color" r.trfm of
              Nothing   -> Color opts xs
              Just trfm -> trfm (Color opts xs))
  go code
  where
  go (Color (ColorOpts opts') xs) = do
    html <- bbcodeToHTML xs
    let color' = (case opts'.colorValue of
               Just (ColorName name)  -> case StrM.lookup name htmlSchemeMap of
                                              Nothing    -> Color.black
                                              Just color -> color

               Just (ColorHex hex)    -> case Color.fromHexString hex of
                                              Nothing    -> Color.black
                                              Just color -> color
               _                      -> Color.black) -- TODO FIXME: default
    pure $ H.span [CSS.style $ CSS.color color'] html
  go _ = pure $ H.div_ []



runImage :: forall p i. ImageOpts -> MediaURL -> ParseEff (HTML p (i Unit))
runImage opts url = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "img" r.trfm of
             Nothing   -> Image opts url
             Just trfm -> trfm (Image opts url))
  go code
  where
  go (Image (ImageOpts opts) url) = do
    let
      height_props = case opts.imageHeight of
                          Nothing -> []
                          Just (ImagePx n)      -> [P.height $ P.Pixels n]
                          Just (ImagePercent n) -> [P.height $ P.Percent n]
      width_props = case opts.imageWidth of
                         Nothing -> []
                         Just (ImagePx n)      -> [P.width $ P.Pixels n]
                         Just (ImagePercent n) -> [P.width $ P.Percent n]
    let props = height_props <> width_props
    pure (H.img $ props <> [P.src url])
  go _ = pure $ H.div_ []
