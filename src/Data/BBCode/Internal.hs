module Data.BBCode.Internal (
  Unit,
  List,
  Tuple,
  tuple,
  (<<<)
) where



type Unit      = ()
type List a    = [a]
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)


infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)



