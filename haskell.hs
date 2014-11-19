isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0  = True
  | otherwise       = False


isEven' n = n `mod` 2 == 0


rev [] = []
rev (x:xs) = rev xs ++ [x]

addToAll x = [ n + 1 | n <- x]

square x = [n * n | n <- x]

rev' xs = revAcc [] xs
revAcc _ [] = []
revAcc acc r@(x:xs) 
    | null r   = acc
    | otherwise = revAcc ([x] ++ acc) (xs)
