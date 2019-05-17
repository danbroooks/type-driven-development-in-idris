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

myLength : List a -> Nat
myLength [] = 0
myLength (_ :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

myVectMap : (a -> b) -> Vect n a -> Vect n b
myVectMap f [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

prependVect : (x : Vect n elem) -> (xs : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
prependVect [] [] = []
prependVect (x :: xs) (y :: ys) = (x :: y) :: prependVect xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = prependVect x (transposeMat xs)
