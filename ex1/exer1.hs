det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b -sqrt(det a b c))/2*a
quadsol2 a b c = (-b +sqrt(det a b c))/2*a


third_a :: [a] -> a
third_a x = x!!2
third_b :: [a] -> a
third_b (x:y:z:xs) =z

fact :: (Num a, Eq a) => a -> a
fact 0 = 1
fact x = x * fact (x-1)

hailstone :: (Integral a, Eq a) => a -> a
hailstone n 
 | n `mod` 2==0  = n `div` 2
 | otherwise = 3*n+1



hailLen 1 = 0
hailLen x = 1 + hailLen (hailstone x)
