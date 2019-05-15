top_ten : Ord a => List a -> List a
top_ten as = take 10 $ reverse (sort as)
