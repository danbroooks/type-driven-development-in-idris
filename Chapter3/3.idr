import Data.Vect
import Chapter3.TransposeMat

mulVects : Num a => Vect m a -> Vect m a -> a
mulVects a b = sum (zipWith (*) a b)

helperFunc : Num a => Vect m a -> Vect p (Vect m a) -> Vect p a
helperFunc _ [] = []
helperFunc x (y :: ys) = mulVects x y :: helperFunc x ys

multMatrixHelper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
multMatrixHelper [] _ = []
multMatrixHelper (x :: xs) ys = helperFunc x ys :: multMatrixHelper xs ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = multMatrixHelper xs (transposeMat ys)
