import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate



-- have a merged list
-- time to reorder
-- merge:: Ord a =>[a] -> [a] -> [a]
-- merge xs ys =  makeSortedList [] (xs ++ ys) 


-- -- makeSortedList::[a]->[a]->[a]
-- makeSortedList a [] = a
-- makeSortedList a xs =  makeSortedList  (a++[append]) (filterFirstElement (append) xs )
--   where append=minimum xs

-- --Used built-in haskell filter function first which caused every matching element to be removed except one. Thus, used custom
-- --function to remove first matching element from list.
-- --Inspiration taken from : https://www.educative.io/answers/how-to-remove-an-element-from-a-list-in-haskell
-- filterFirstElement a (x:xs) 
--   | a == x    = xs
--   | otherwise = x : filterFirstElement a xs


-- Referenced from prof's class tip comparing each element at beginning of list and recursing
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xy = xy
merge (x:xs) (y:xy) 
 | x < y = x : merge xs (y:xy)
 | x > y = y : merge (x:xs) xy
 | x == y = x : merge xs (y:xy)
 | otherwise = x : merge xs (y:xy)

mergeSort :: Ord a => [a] -> [a]
-- base cases
mergeSort []  = []
mergeSort [x] = [x]
--recursive case
mergeSort xs = let (leftSubArray,rightSubArray) = splitAt (length(xs) `div` 2) xs
 in merge (mergeSort (leftSubArray)) (mergeSort (rightSubArray))

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31


isFriday  :: Day -> Bool
isFriday y
  | fromEnum(dayOfWeek(y)) == 5 =True
  | otherwise = False

--from ex2 
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

getDay :: (Year, MonthOfYear, DayOfMonth) -> Int
getDay (y,m,d) = d

isPrimeDay  :: Day -> Bool
isPrimeDay y 
    | divisors(getDay(toGregorian(y))) == [] = True
    | otherwise = False

primeFridays :: Integer -> [Day]
primeFridays y =[i | i<-daysInYear(y), isPrimeDay(i),isFriday(i)]
