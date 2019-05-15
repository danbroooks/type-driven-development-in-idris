palindrome : String -> Bool
palindrome str =
  if length str > 10
     then str' == reverse str'
     else False
  where
    str' : String
    str' = toLower str
