module Chapter2.Counts

export
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)
