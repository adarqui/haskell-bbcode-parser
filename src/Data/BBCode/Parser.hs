module Data.BBCode.Parser (
  open,
  closed,
  str,
  token,
  tokens,
  concatTokens,
  concatBBStr,
  runBBCode,
  parseTokens,
  parseTokens',
  parseBBCodeFromTokens,
  parseBBCodeFromTokens',
  parseBBCode,
  parseBBCodeWith
) where



import Control.Alt -- ((<|>))
import Control.Monad.RWS               (evalRWS, modify, gets)
import Data.Either                     (Either(..))
import Data.Foldable                   (class Foldable, foldMap)
import Data.List                       (List(..), filter, uncons, reverse, toUnfoldable, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe                      (Maybe(..))
import Data.String                     (joinWith, toLower)
import Data.String as String
import Data.Tuple                      (Tuple(..), fst, snd)
import Data.Unfoldable                 (replicate)
import Prelude                         (bind, pure, map, show, ($), (-), (>=), (<), (<>)
                                       ,(+), (>), (==), (||), (/=), (&&), (*>), (<<<), (<$>))
import Text.Parsing.Simple             (Parser, parse, try,
                                       char, string, alphanum, letter, int, item
                                       ,isn'tAny, some, many)

import Data.BBCode.Types               (ParseReader, BBDoc, ParseEff, BBCodeMap, ErrorMsg, Parameters, TagName, BBCodeFn
                                       ,BBCode(..), BBColor(..), BBSize(..), ColorOpts(..), FontOpts(..), SizeOpts(..)
                                       ,Token(..), defaultParseState, defaultParseReader, defaultImageOpts, defaultColorOpts
                                       ,defaultSizeOpts, defaultFontOpts)



-- | Parses both [tag] and [tag params] | [tag=params]
--
open :: Parser String Token
open = do
  _ <- string "["
  c <- letter
  r <- many (isn'tAny " =]")
  c' <- (char ' ' <|> char '=' <|> char ']')
  case c' of
    ']' -> pure $ BBOpen Nothing (fromCharListToLower $ c : r)
    _   -> do
          pc <- item
          pr <- many (isn'tAny "]")
          _ <- char ']'
          pure $ BBOpen (Just (fromCharList $ pc : pr)) (fromCharListToLower $ c : r)



closed :: Parser String Token
closed = do
  _ <- string "[/"
  c <- letter
  r <- many (isn'tAny "]")
  _ <- char ']'
  pure $ BBClosed (fromCharListToLower $ c : r)



str :: Parser String Token
str = do
  r <- some (isn'tAny "[]")
  pure $ BBStr (fromCharList r)



stringLiteral :: Parser String String
stringLiteral = do
  _ <- char '"'
  s <- fromCharList <$> some (isn'tAny "\"")
  _ <- char '"'
  pure s



identifier :: Parser String String
identifier = do
  fromCharList <$> some (isn'tAny " ")



catchAll :: Parser String Token
catchAll = do
  r <- some item
  pure $ BBStr (fromCharList r)



token :: Parser String Token
token = do
  try closed <|> try open <|> try str <|> try catchAll



tokens :: Parser String (List Token)
tokens = many token



fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap String.singleton



fromCharListToLower :: forall f. Foldable f => f Char -> String
fromCharListToLower = toLower <<< fromCharList



-- | concat consecutive BBStr's
--
concatTokens :: List Token -> List Token
concatTokens = go Nil
  where
  go accum xs =
    case uncons xs of
      Nothing             -> reverse accum
      Just { head, tail } ->
        let
          heads = L.takeWhile isBBStr tail
          tails = L.dropWhile isBBStr tail
        in
          if isBBStr head
                         then go ((concatBBStr $ head : heads) : accum) tails
                         else go (head : accum) tail



-- | Once we have a list of BBStr's, turn them into one BBStr
--
concatBBStr :: List Token -> Token
concatBBStr = BBStr <$> joinWith "" <<< toUnfoldable <<< map go <<< filter isBBStr
  where
  go (BBStr s) = s
  go _         = ""



isBBStr :: Token -> Boolean
isBBStr (BBStr _) = true
isBBStr _         = false



parseTokens :: forall s. s -> Parser s (List Token) -> Either String (List Token)
parseTokens input p =
  case parse p input of
    Left err     -> Left $ show err
    Right actual -> Right actual



parseTokens' :: String -> Either String (List Token)
parseTokens' s = parseTokens s tokens



runBBCode :: Maybe Parameters -> TagName -> List BBCode -> BBCodeMap -> Either ErrorMsg BBCode
runBBCode params tag doc bmap  =
  case M.lookup tag bmap of
       Nothing   -> Left $ tag <> " not found"
       Just bbFn -> bbFn params doc



runBold :: BBCodeFn
runBold = runTextSimple Bold "Bold"

runItalic :: BBCodeFn
runItalic = runTextSimple Italic "Italic"

runUnderline :: BBCodeFn
runUnderline = runTextSimple Underline "Underline"

runStrike :: BBCodeFn
runStrike = runTextSimple Strike "Strike"

runFont :: BBCodeFn
runFont m_params xs =
  case m_params of
       Nothing   -> Right $ Font defaultFontOpts xs
       Just font -> Right $ Font (FontOpts { fontFamily: Just font, fontFaces: [] }) xs
       -- TODO FIXME: font faces

runSize :: BBCodeFn
runSize m_params xs =
  case m_params of
       Nothing -> Right $ Size defaultSizeOpts xs
       Just sz ->
        -- simple parsing
        let lr = parse parseBBSize sz in
        case lr of
             Left _  -> Right $ Size defaultSizeOpts xs
             Right v -> Right $ Size (SizeOpts { sizeValue: Just v }) xs
  where
  parseBBSize = try px <|> try pt <|> try em
  px = do
    n <- int
    _ <- string "px"
    pure $ SizePx n
  pt = do
    n <- int
    _ <- string "pt"
    pure $ SizePt n
  em = do
    n <- int
    _ <- string "em"
    pure $ SizeEm n



runColor :: BBCodeFn
runColor m_params xs =
  case m_params of
       Nothing -> Right $ Color defaultColorOpts xs
       Just sz ->
        -- simple parsing
        let lr = parse parseBBColor sz in
        case lr of
             Left _  -> Right $ Color defaultColorOpts xs
             Right v -> Right $ Color (ColorOpts { colorValue: Just v }) xs
  where
  parseBBColor = try quoted_name <|> try hex <|> try name
  quoted_name = do
    name' <- stringLiteral
    pure $ ColorName name'
  hex = do
    hex_code <- char '#' *> some alphanum
    pure $ ColorHex (fromCharList $ '#' : hex_code)
  name = do
    name' <- identifier
    pure $ ColorName name'



runCenter :: BBCodeFn
runCenter = runTextSimple Center "Center"

runAlignLeft :: BBCodeFn
runAlignLeft = runTextSimple AlignLeft "Left"

runAlignRight :: BBCodeFn
runAlignRight = runTextSimple AlignRight "Right"

runQuote :: BBCodeFn
runQuote = runTextSimple (Quote Nothing) "Quote"

runLink :: BBCodeFn
runLink m_params (Cons (Text s) Nil) =
  case m_params of
       Nothing   -> Right $ Link Nothing s
       Just url  -> Right $ Link (Just s) url
runLink _ _ = Left $ "Link" <> " error"

runPre :: BBCodeFn
runPre = runRaw Pre "Pre"

runCode :: BBCodeFn
runCode = runRaw (Code Nothing) "Code"

runMove :: BBCodeFn
runMove = runTextSimple Move "Move"

runNL :: BBCodeFn
runNL _ Nil = Right $ NL
runNL _ _   = Left $ "nl error"

runHR :: BBCodeFn
runHR _ Nil = Right $ HR
runHR _ _   = Left "hr error"

--
-- TODO FIXME: media needs proper url parsing/verification
--

runYoutube :: BBCodeFn
runYoutube = runMedia Youtube "Youtube"

runVimeo :: BBCodeFn
runVimeo = runMedia Vimeo "Vimeo"

runFacebook :: BBCodeFn
runFacebook = runMedia Facebook "Facebook"

runInstagram :: BBCodeFn
runInstagram = runMedia Instagram "Instagram"

runStreamable :: BBCodeFn
runStreamable = runMedia Streamable "Streamable"

runImgur :: BBCodeFn
runImgur = runMedia Imgur "Imgur"

runImage :: BBCodeFn
runImage = runMedia (Image defaultImageOpts) "Image"


-- Helpers
--

runTextSimple :: (List BBCode -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runTextSimple _ tag _ Nil = Left $ tag <> " error"
runTextSimple mk _ _ t    = Right $ mk t

runRaw :: (String -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runRaw mk _ _ (Cons (Text raw) Nil) = Right $ mk raw
runRaw _ tag _ _                    = Left $ tag <> " error"

runMedia :: (String -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runMedia mk _ _ (Cons (Text url) Nil)  = Right $ mk url
runMedia _ tag _ (Cons _ Nil)          = Left $ tag <> " error: only urls may be wrapped in " <> tag
runMedia _ tag _ _                     = Left $ tag <> " error"



-- | The default BBCode map specifies all bbcode that works within open & closed tags
--
defaultBBCodeMap :: BBCodeMap
defaultBBCodeMap =
  M.fromFoldable [
    Tuple "b" runBold,
    Tuple "i" runItalic,
    Tuple "u" runUnderline,
    Tuple "s" runStrike,
    Tuple "font" runFont,
    Tuple "size" runSize,
    Tuple "color" runColor,
    Tuple "center" runCenter,
    Tuple "left" runAlignLeft,
    Tuple "right" runAlignRight,
    Tuple "quote" runQuote,
    Tuple "link" runLink,
    Tuple "url" runLink,
--    Tuple "list" runList,
--    Tuple "ol" runOrdList,
--    Tuple "ordlist" runOrdList,
--    Tuple "table" runTable,
    Tuple "move" runMove,
    Tuple "img" runImage,
    Tuple "youtube" runYoutube,
    Tuple "vimeo" runVimeo,
    Tuple "facebook" runFacebook,
    Tuple "instagram" runInstagram,
    Tuple "streamable" runStreamable,
    Tuple "imgur" runImgur
  ]



-- | The unary map is for bbcode that doesn't have a closing tag
--
defaultUnaryBBCodeMap :: BBCodeMap
defaultUnaryBBCodeMap =
  M.fromFoldable [
    Tuple "hr" runHR
  ]



-- | The "Consume" map is for bbcode that consumes all other tags up until its own closing tag
--
defaultConsumeBBCodeMap :: BBCodeMap
defaultConsumeBBCodeMap =
  M.fromFoldable [
    Tuple "pre" runPre,
    Tuple "code" runCode
  ]



-- | TODO FIXME: worst function ever.. my brain is not working
--
parseTextAndNewlines :: String -> List BBCode
parseTextAndNewlines = go Nil
  where
  go acc "" = acc
  go acc s  =
    let
      str'   = String.takeWhile (\c -> c /= '\r' && c /= '\n') s
      nl     = if (String.length str' == 0)
                  then String.length $ String.takeWhile (\c -> c == '\r' || c == '\n') s
                  else 0
      rest   = if (nl > 0)
                  then String.drop nl s
                  else String.drop (String.length str') s
    in
      if (nl > 0)
         then
           go (replicate nl NL <> acc) rest
         else
           go (Text str' : acc) rest



parseBBCodeFromTokens :: List Token -> ParseEff (Either String BBDoc)
parseBBCodeFromTokens = parseBBCodeFromTokens' defaultBBCodeMap defaultUnaryBBCodeMap defaultConsumeBBCodeMap



parseBBCodeFromTokens' ::
     BBCodeMap
  -> BBCodeMap
  -> BBCodeMap
  -> List Token
  -> ParseEff (Either String BBDoc)
parseBBCodeFromTokens' bmap umap cmap toks = go toks 0
  where

  try_maps params tag =
    case M.lookup tag bmap, M.lookup tag cmap of
         Just bmap_fn, Nothing -> \xs -> runBBCode params tag xs bmap
         Nothing, Just cmap_fn -> \xs -> runBBCode params tag xs cmap
         -- TODO FIXME: need a user supplied FN to handle errors, this is what runBBCode was for; but not anymore
         _, _                  -> \xs -> Right $ Text tag

  go :: List Token -> Int -> ParseEff (Either String BBDoc)
  go toks' level = do

    stack <- gets _.stack
    accum <- gets _.accum
    saccum <- gets _.saccum

    case uncons toks' of
      Nothing -> do
        case stack of
          Nil                    -> pure $ Right $ reverse accum
          (Cons (Tuple _ tag) _) -> pure $ Left $ tag <> " not closed"
      Just { head, tail } ->
        case head of

          BBStr s           -> do
            let
              text_and_newlines = parseTextAndNewlines s
            if L.null stack
               then do
                 modify (\st -> st{ accum = text_and_newlines <> st.accum })
                 go tail level
               else do
                 modify (\st -> st{ saccum = (map (Tuple level) text_and_newlines) <> st.saccum })
                 go tail level

          BBOpen params tag -> do
            -- We need to handle things differently based upon whether or not the bbcode is:
            -- 1. a unary operator - no closing tag
            -- 2. a consumer - consumes all other tags until the consumer's closing tag is found
            -- 3. a normal bbcode operator which has an open tag, content, and a closing tag
            if M.member tag umap
               then do
                 case (runBBCode params tag Nil umap) of
                   Left err   -> pure $ Left err
                   Right new' -> do
                     modify (\st -> st{ accum = (new' : st.accum) })
                     go tail level
               else do
                 modify (\st -> st{ stack = (Tuple params tag) : st.stack })
                 go tail (level+1)

          BBClosed tag      -> do
            case uncons stack of
              Nothing -> pure $ Left $ tag <> " not pushed"
              Just { head: Tuple params tag, tail : stTail } -> do
                let
                  beneath = filter (\(Tuple l v) -> l < level) saccum
                  at_or_above = filter (\(Tuple l v) -> l >= level) saccum
                case (try_maps params tag (L.reverse $ map snd at_or_above)) of
                  Left err -> pure $ Left err
                  Right new' -> do
                    if L.null stTail
                       then do
                         modify (\st -> st{ accum = new' : st.accum, stack = stTail, saccum = Nil :: (List (Tuple Int BBCode)) })
                         go tail (level-1)
                       else do
                         modify (\st -> st{ saccum = (Tuple level new' : beneath), stack = stTail })
                         go tail (level-1)



parseBBCode :: String -> Either String (List BBCode)
parseBBCode = parseBBCodeWith defaultParseReader



parseBBCodeWith :: ParseReader -> String -> Either String (List BBCode)
parseBBCodeWith parse_reader s =
  case toks of
       Left s   -> Left s
       Right bb -> fst $ evalRWS (parseBBCodeFromTokens bb) parse_reader defaultParseState
  where
  toks = parseTokens' s
