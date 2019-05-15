module Main

import Chapter2.Counts
import Chapter2.Palindrome

presentResult : String -> String
presentResult input = unlines
  [ "Palindrome: " ++ show (palindrome 0 input)
  , "Counts: " ++ show (counts input)
  ]

main : IO ()
main = repl "Enter a string: " presentResult
