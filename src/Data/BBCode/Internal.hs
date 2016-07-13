module Data.BBCode.Internal (
  List,
  Tuple,
  tuple
) where



type List a    = [a]
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)
