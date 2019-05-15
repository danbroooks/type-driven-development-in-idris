palindrome : String -> Bool
palindrome str = str' == reverse str'
  where
    str' : String
    str' = toLower str
