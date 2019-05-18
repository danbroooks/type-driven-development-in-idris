import Data.Vect
import Chapter3.TransposeMat

dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct a b = sum (zipWith (*) a b)

applyDotProduct : Num a => Vect n a -> Vect m (Vect n a) -> Vect m a
applyDotProduct x ys = dotProduct x <$> ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = flip applyDotProduct (transposeMat ys) <$> xs
