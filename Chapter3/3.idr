import Data.Vect
import Chapter3.TransposeMat

vectorProduct : Num a => Vect m a -> Vect p (Vect m a) -> Vect p a
vectorProduct _ [] = []
vectorProduct x (y :: ys) = sum (zipWith (*) x y) :: vectorProduct x ys

multMatrixHelper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multMatrixHelper [] _ = []
multMatrixHelper (x :: xs) ys = vectorProduct x ys :: multMatrixHelper xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multMatrixHelper xs (transposeMat ys)
