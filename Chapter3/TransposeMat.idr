module Chapter3.TransposeMat

import Data.Vect

export
total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)
