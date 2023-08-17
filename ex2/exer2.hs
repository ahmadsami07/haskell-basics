divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean c = [(a,b,c) | a <- [1..c],a>0, b <- [1..c],b>0, c <- [1..c],c>0, c*c == a*a +b*b,a<b]

-- Used tail recursion using a as the accumulator in the join' function
-- join p x 
--  | x == [] =""
--  | otherwise = join' "" p x

-- join' a p (x:xs)
--  | xs == []    = a ++ x
--  | otherwise = join' new p xs
--  where new = a ++ x ++ p


join p [] = ""
join p [x] = x
join p (x:xs) = (++) x p ++ join p xs 

join':: String -> [String] -> String
join' sep [x] = x
join' sep (x:xs) = x ++ sep ++ join' sep xs


fact' x = foldl (*) 1 [n | n <- [1..x]]

-- From ex 1
hailstone n 
 | n `mod` 2==0  = n `div` 2
 | otherwise = 3*n+1


hailLen n = hailTail 0 n
  where
  hailTail a 1 = a 
  hailTail a x = hailTail (a+1) (hailstone(x))



