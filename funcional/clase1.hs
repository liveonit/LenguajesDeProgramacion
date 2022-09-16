allEqual :: (Eq a) => [a] -> Bool
allEqual xs
  | length(xs) == 0 = error "Lista vacia"
  | length(xs) == 1 = error "Lista con 1 elemento"
  | length(xs) == 2 = xs !! 0 == xs !! 1
  | otherwise = (xs !! 0 == xs !! 1) && (allEqual (tail xs))