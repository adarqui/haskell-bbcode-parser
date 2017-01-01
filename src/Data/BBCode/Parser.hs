{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.BBCode.Parser (
  open,
  closed,
  str,
  token,
  tokens,
  concatTokens,
  concatBBStr,
  splitParams,
  splitKVs,
  buildParamMap,
  runBBCode,
  parseTokens,
  parseTokens',
  parseBBCodeFromTokens,
  parseBBCodeFromTokens',
  parseBBCode,
  parseBBCodeWith,
  textAndEmoticons
) where



import           Control.Applicative  ((<*), (<|>))
import           Control.Monad.RWS    (evalRWS, gets, modify, asks)
import           Data.Attoparsec.Text
import           Data.Char            (isAlpha, isAlphaNum, isSpace)
import           Data.Either          (Either (..), rights)
import qualified Data.List            as List
import           Data.Bimap           (Bimap)
import qualified Data.Bimap           as Bimap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (Maybe (..))
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Tuple           (fst, snd)
import           Prelude              (Bool (..), Char, Int, const, map, not,
                                       otherwise, pure, show, undefined, ($),
                                       (&&), (*>), (+), (-), (/=), (<), (<$>),
                                       (==), (>), (>=), (||), error, fmap)

import           Data.BBCode.Internal
import           Data.BBCode.Types



alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum



noneOf :: Text -> Char -> Bool
noneOf s c = Text.all (/= c) s



-- | Basically, takeWhile1 (\c -> test c && noneOf s)
--
noneOfAndAllOf :: Text -> (Char -> Bool) -> Char -> Bool
noneOfAndAllOf s test c = test c && noneOf s c



-- | Parses both [tag] and [tag params] | [tag=params]
--
open :: Parser Token
open = do
  _  <- char '['
  bb <- takeWhile1 (noneOfAndAllOf " =]" isAlpha)
  c' <- (char ' ' <|> char '=' <|> char ']')
  case c' of
    ']' -> pure $ BBOpen Nothing (Text.toLower $ bb)
    _   -> do
          params <- takeWhile1 (/= ']')
          _      <- char ']'
          pure $ BBOpen (Just params) (Text.toLower bb)



-- | Parses a closing tag: [/tag]
-- Closing tags may also contain unparsed "meta data", for example:
-- [quote author=adarqui link=poop date=2016 id=111]hello![/quote id=111]
--
closed :: Parser Token
closed = do
  _  <- string "[/"
  bb <- takeWhile1 isAlphaNum
  _  <- takeWhile (/= ']')
  _  <- char ']'
  pure $ BBClosed (Text.toLower bb)



str :: Parser Token
str = do
  r <- takeWhile1 (noneOf "[]")
  pure $ BBStr r



stringLiteral :: Parser Text
stringLiteral = do
  _ <- char '"'
  s <- takeWhile1 (/= '"')
  _ <- char '"'
  pure s



identifier :: Parser Text
identifier = takeWhile1 (not <<< isSpace)



catchAll :: Parser Token
catchAll = do
  r <- takeWhile1 (const True)
  pure $ BBStr r



token :: Parser Token
token = do
  try closed <|> try open <|> try str <|> try catchAll



tokens :: Parser (List Token)
tokens = many' token



-- | concat consecutive BBStr's
--
concatTokens :: List Token -> List Token
concatTokens = go Nil
  where
  go accum xs =
    case List.uncons xs of
      Nothing             -> List.reverse accum
      Just (head, tail) ->
        let
          heads = List.takeWhile isBBStr tail
          tails = List.dropWhile isBBStr tail
        in
          if isBBStr head
                         then go ((concatBBStr $ head : heads) : accum) tails
                         else go (head : accum) tail



-- | Once we have a list of BBStr's, turn them into one BBStr
--
concatBBStr :: List Token -> Token
concatBBStr = BBStr <$> Text.concat <<< map go <<< List.filter isBBStr
  where
  go (BBStr s) = s
  go _         = ""



isBBStr :: Token -> Boolean
isBBStr (BBStr _) = true
isBBStr _         = false



-- | Split text into unparsed params
--
splitParams :: Text -> Either [Char] [Text]
splitParams = parseOnly (sepBy (takeWhile1 (/= ' ')) (char ' '))



-- | Split unparsed k=v's into (k,v)'s
--
--
-- splitKVs :: Either [Char] [Text] -> Either [Char] [Text]
-- splitKVs = fmap (parseOnly (sepBy (many1 (satisfy isAlphaNum)) (char '=')))
-- splitKVs = fmap (parseOnly (sepBy (takeWhile1 isAlphaNum) (char '=')))
-- splitKVs (Left e) = Left e
splitKvs (Left  v) = Left v
splitKVs (Right v) = rights $ map (\s -> toTup $ parseOnly (sepBy (takeWhile1 acceptableChars) (char '=')) s) v
  where
  toTup (Right (x:y:[])) = Right (x, y)
  toTup _                = Left "toTup error"
  acceptableChars c = c /= '='



-- | Builds a map of key/value params
--
buildParamMap :: Text -> Map Text Text
buildParamMap params =
  case splitKVs (splitParams params) of
    []  -> Map.empty
    kvs -> Map.fromList kvs



parseTokens :: Text -> Parser (List Token) -> Either Text (List Token)
parseTokens input p =
  case parseOnly (p <* endOfInput) input of
    Left err     -> Left $ Text.pack err
    Right actual -> Right actual



parseTokens' :: Text -> Either Text (List Token)
parseTokens' s = parseTokens s tokens



runBBCode :: Maybe Parameters -> TagName -> List BBCode -> BBCodeMap -> Either ErrorMsg BBCode
runBBCode params tag doc bmap  =
  case Map.lookup tag bmap of
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
       Just font -> Right $ Font (FontOpts { fontFamily = Just font, fontFaces = Nil }) xs
       -- TODO FIXME: font faces

runSize :: BBCodeFn
runSize m_params xs =
  case m_params of
       Nothing -> Right $ Size defaultSizeOpts xs
       Just sz ->
        -- simple parsing
        let lr = parseOnly parseBBSize sz in
        case lr of
             Left _  -> Right $ Size defaultSizeOpts xs
             Right v -> Right $ Size (SizeOpts { sizeValue = Just v }) xs
  where
  parseBBSize = try px <|> try pt <|> try em
  px = do
    n <- decimal
    _ <- string "px"
    pure $ SizePx n
  pt = do
    n <- decimal
    _ <- string "pt"
    pure $ SizePt n
  em = do
    n <- decimal
    _ <- string "em"
    pure $ SizeEm n



runColor :: BBCodeFn
runColor m_params xs =
  case m_params of
       Nothing -> Right $ Color defaultColorOpts xs
       Just sz ->
        -- simple parsing
        let lr = parseOnly parseBBColor sz in
        case lr of
             Left _  -> Right $ Color defaultColorOpts xs
             Right v -> Right $ Color (ColorOpts { colorValue = Just v }) xs
  where
  parseBBColor = try quoted_name <|> try hex <|> try name
  quoted_name = do
    name' <- stringLiteral
    pure $ ColorName name'
  hex = do
    hex_code <- char '#' *> takeWhile1 isAlphaNum
    pure $ ColorHex (Text.cons '#' hex_code)
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
runQuote m_params xs =
  case m_params of
       -- Nothing     -> runTextSimple (Quote Nothing Nothing Nothing Nothing) "Quote" Nothing xs
       -- Just params -> runTextSimple (Quote m_author m_avatar m_link m_date) "Quote" Nothing xs
       Nothing     -> Right $ Quote Nothing Nothing Nothing Nothing xs
       Just params -> Right $ Quote m_author m_avatar m_link m_date xs
        where
        param_map = buildParamMap params
        m_author  = Map.lookup "author" param_map
        m_avatar  = Map.lookup "avatar" param_map
        m_link    = Map.lookup "link" param_map
        m_date    = Map.lookup "date" param_map

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

runRaw :: (Text -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runRaw mk _ _ (Cons (Text raw) Nil) = Right $ mk raw
runRaw _ tag _ _                    = Left $ tag <> " error"

runMedia :: (Text -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runMedia mk _ _ (Cons (Text url) Nil)  = Right $ mk url
runMedia _ tag _ (Cons _ Nil)          = Left $ tag <> " error: only urls may be wrapped in " <> tag
runMedia _ tag _ _                     = Left $ tag <> " error"



-- | The default BBCode map specifies all bbcode that works within open & closed tags
--
defaultBBCodeMap :: BBCodeMap
defaultBBCodeMap =
  Map.fromList [
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
  Map.fromList [
    Tuple "hr" runHR
  ]



-- | The "Consume" map is for bbcode that consumes all other tags up until its own closing tag
--
defaultConsumeBBCodeMap :: BBCodeMap
defaultConsumeBBCodeMap =
  Map.fromList [
    Tuple "pre" runPre,
    Tuple "code" runCode
  ]



-- | TODO FIXME: worst function ever.. my brain is not working
--
parseTextAndNewlines :: Text -> List BBCode
parseTextAndNewlines = go Nil
  where
  go acc "" = acc
  go acc s  =
    let
      str'   = Text.takeWhile (\c -> c /= '\r' && c /= '\n') s
      nl     = if (Text.length str' == 0)
                  then Text.length $ Text.takeWhile (\c -> c == '\r' || c == '\n') s
                  else 0
      rest   = if (nl > 0)
                  then Text.drop nl s
                  else Text.drop (Text.length str') s
    in
      if (nl > 0)
         then
           go (List.replicate nl NL <> acc) rest
         else
           go (Text str' : acc) rest



-- | Pull emoticons out of Text
--
textAndEmoticons :: Bimap Text Text -> List BBCode -> List BBCode
textAndEmoticons emoticon_map xs = go emoticons xs
  where
  emoticons                      = Bimap.toList emoticon_map
  go [] codes                    = codes
  go ((emot,emot_key):emots) codes = let
                                       new_codes = map (\code -> case code of
                                                                   Text text -> case Text.splitOn emot text of
                                                                                  []     -> [code]
                                                                                  broken -> List.filter (/= Text "") $ List.intersperse (Emoticon emot_key) (map Text broken)
                                                                   other     -> [other]) codes
                                     in go emots (List.concat new_codes)



parseBBCodeFromTokens :: List Token -> ParseEff (Either Text BBDoc)
parseBBCodeFromTokens = parseBBCodeFromTokens' defaultBBCodeMap defaultUnaryBBCodeMap defaultConsumeBBCodeMap



parseBBCodeFromTokens' ::
     BBCodeMap
  -> BBCodeMap
  -> BBCodeMap
  -> List Token
  -> ParseEff (Either Text BBDoc)
parseBBCodeFromTokens' bmap umap cmap toks = do
  lr_parsed       <- go toks 0
  m_emoticons_map <- asks emoticons
  case (m_emoticons_map, lr_parsed) of
    (Just (emoticons_bimap, _), Right bb_doc) -> pure $ Right $ textAndEmoticons emoticons_bimap bb_doc
    _                                         -> pure lr_parsed
  where

  try_maps params tag =
    case (Map.lookup tag bmap, Map.lookup tag cmap) of
       (Just bmap_fn, Nothing) -> \xs -> runBBCode params tag xs bmap
       (Nothing, Just cmap_fn) -> \xs -> runBBCode params tag xs cmap
       -- TODO FIXME: need a user supplied FN to handle errors, this is what runBBCode was for; but not anymore
       _                       -> \xs -> Right $ Text tag

  go :: List Token -> Int -> ParseEff (Either Text BBDoc)
  go toks' level = do

    stack <- gets stack
    accum <- gets accum
    saccum <- gets saccum

    case List.uncons toks' of
      Nothing -> do
        case stack of
          Nil                    -> pure $ Right $ List.reverse accum
          (Cons (Tuple _ tag) _) -> do
            allow_not_closed <- asks allowNotClosed
            if allow_not_closed
              then pure $ Right $ List.reverse $ Text ("["<>tag<>"]") : accum
              else pure $ Left $ tag <> " not closed"
      Just (head, tail) ->
        case head of

          BBStr s           -> do
            let
              text_and_newlines = parseTextAndNewlines s
            if List.null stack
               then do
                 modify (\st@ParseState{..} -> st{ accum = text_and_newlines <> accum })
                 go tail level
               else do
                 modify (\st@ParseState{..} -> st{ saccum = (map (Tuple level) text_and_newlines) <> saccum })
                 go tail level

          BBOpen params tag -> do
            -- We need to handle things differently based upon whether or not the bbcode is:
            -- 1. a unary operator - no closing tag
            -- 2. a consumer - consumes all other tags until the consumer's closing tag is found
            -- 3. a normal bbcode operator which has an open tag, content, and a closing tag
            if Map.member tag umap
               then do
                 case (runBBCode params tag Nil umap) of
                   Left err   -> pure $ Left err
                   Right new' -> do
                     modify (\st@ParseState{..} -> st{ accum = (new' : accum) })
                     go tail level
               else do
                 if Map.member tag bmap || Map.member tag cmap
                   then do
                     modify (\st@ParseState{..} -> st{ stack = (Tuple params tag) : stack })
                     go tail (level+1)
                   else do
                     -- tag not found
                     modify (\st@ParseState{..} -> st { accum = Text ("["<>tag<>"]") : accum })
                     go tail level

          BBClosed tag      -> do
            case List.uncons stack of
              Nothing -> pure $ Left $ tag <> " not pushed"
              Just ((params, tag), stTail) -> do
                let
                  beneath = List.filter (\(Tuple l v) -> l < level) saccum
                  at_or_above = List.filter (\(Tuple l v) -> l >= level) saccum
                case (try_maps params tag (List.reverse $ map snd at_or_above)) of
                  Left err -> pure $ Left err
                  Right new' -> do
                    if List.null stTail
                       then do
                         modify (\st@ParseState{..} -> st{ accum = new' : accum, stack = stTail, saccum = Nil :: (List (Tuple Int BBCode)) })
                         go tail (level-1)
                       else do
                         modify (\st -> st{ saccum = (Tuple level new' : beneath), stack = stTail })
                         go tail (level-1)



parseBBCode :: Text -> Either Text (List BBCode)
parseBBCode = parseBBCodeWith defaultParseReader



parseBBCodeWith :: ParseReader -> Text -> Either Text (List BBCode)
parseBBCodeWith parse_reader s =
  case toks of
       Left s   -> Left s
       Right bb -> fst $ evalRWS (parseBBCodeFromTokens bb) parse_reader defaultParseState
  where
  toks = parseTokens' s
