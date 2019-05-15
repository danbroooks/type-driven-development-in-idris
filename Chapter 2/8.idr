count_longer_than : Nat -> String -> Nat
count_longer_than min str =
  if length str > min then 1 else 0

over_length : Nat -> List String -> Nat
over_length min = foldr ((+) . count_longer_than min) 0
