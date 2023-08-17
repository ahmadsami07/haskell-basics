pascal :: (Eq t, Num t, Num a) => t -> [a]
pascal 0 = [1]
pascal n = pascalRecur n 0 []


pascalRecur :: (Eq t, Num t, Num a) => t -> t -> [a] -> [a]
pascalRecur n a prev
 | n == a = prev
 | n == 1 = pascalRecur n 1 [1,1]
 | otherwise = pascalRecur n (a+1) newList
 where newList = tupleAdd (zip prev (tail prev)) []

tupleAdd :: Num a => [(a, a)] -> [a] -> [a]
tupleAdd [] newList =  1 : newList ++ [1]
tupleAdd ((x1,y1):xs) newList =  tupleAdd xs (newList ++ [prevAdd])
 where prevAdd = x1+y1

addPair :: (Integer, Integer) -> Integer
addPair = uncurry (+)

-- zeroChecker [] = []
-- zeroChecker(x:xs)
--  | x==0 = zeroChecker xs
--  | otherwise = zeroChecker unchangedList
--  where unchangedList = x:xs

withoutZeros:: (Num a, Eq a) => [a] -> [a]
withoutZeros = filter (/=0)

findElt :: (Eq a, Num b) => a -> [a] -> Maybe b
findElt n (x:xs) = findEltRecur n (x:xs) 0

findEltRecur :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> Maybe t2
findEltRecur n [] a = Nothing
findEltRecur n (x:xs) a 
 | n==x = Just a
 | n/=x = findEltRecur n xs (a+1)

findElt' :: (Eq a, Num b) => a -> [a] -> Maybe b
findElt' n (x:xs) = findEltRecur' n (x:xs) 0
  where findEltRecur' n [] a = Nothing
        findEltRecur' n (x:xs) a 
          | n==x = Just a
          | n/=x = findEltRecur' n xs (a+1)

