import Data.Vect

total allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

total allLengths' : Vect len String -> Vect len Nat
allLengths' [] = []
allLengths' (word :: words) = length word :: allLengths' words

clear : Vect len a -> Vect Z a
clear _ = []

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = insert x (insSort xs)
  where
    insert : Ord elem => elem -> Vect n elem -> Vect (S n) elem
    insert a [] = [a]
    insert a (b :: bs) =
      if a < b
         then a :: b :: bs
         else b :: insert a bs
