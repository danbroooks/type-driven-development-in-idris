module Chapter2.Palindrome

export
palindrome : Nat -> String -> Bool
palindrome min str =
  if length str > min
     then str' == reverse str'
     else False
  where
    str' : String
    str' = toLower str
