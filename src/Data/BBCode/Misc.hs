module Data.BBCode.Misc (
  intersperse
) where



import Data.Foldable (foldr)
import Data.List     (List(..), (:))



-- | Ripped directly from Elm.List
--
intersperse :: ∀ a. a -> List a -> List a
intersperse sep xs =
  case xs of
      Nil -> Nil
      Cons hd tl ->
          let
              step x rest =
                  sep : x : rest

              spersed =
                  foldr step Nil tl
          in
              hd : spersed
