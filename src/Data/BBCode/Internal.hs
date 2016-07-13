{-# LANGUAGE PatternSynonyms #-}

module Data.BBCode.Internal (
  Unit,
  List,
  Nil,
  Tuple,
  tuple,
  StrMap,
  pattern Tuple,
  pattern Cons,
  pattern Nil,
  Boolean,
  true,
  false,
  (<<<)
) where



import qualified Data.Map  as M
import           Data.Text (Text)



type Unit      = ()
type List a    = [a]
type Nil       = []
type Tuple a b = (a, b)
type Boolean   = Bool
type StrMap    = M.Map Text



tuple :: a -> b -> (a, b)
tuple = (,)


infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)



pattern Tuple a b = (a, b)
pattern Cons a a' = (:) a a'
pattern Nil       = []



true :: Boolean
true = True



false :: Boolean
false = False
