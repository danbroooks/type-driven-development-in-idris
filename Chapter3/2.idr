import Data.Vect

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix a b = zipWith (zipWith (+)) a b
