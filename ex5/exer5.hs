import Data.Ratio
import Data.List

myIterate :: (a -> a) -> a -> [a] 
myIterate f x =  x : myIterate f functionAppliedToX
 where functionAppliedToX = f x

-- mySplitAt :: Int -> [a] -> ([a], [a]) 
mySplitAt :: (Num t, Eq t, Eq a) => t -> [a] -> ([a], [a])
mySplitAt 0 xs = ([],xs)
mySplitAt n xs = splitListRecur n [] xs

splitListRecur n y (x:z)
 | n==0 = (y,x:z)
 | z == [] = (y++[x],[])
 | otherwise = splitListRecur (n-1) (y ++ [x]) z

rationalSum :: Integral a => a -> [Ratio a]
rationalSum n = [ i % j | i <- [1..n], j <- [1..n], i+j == n]

rationalSumLowest :: Integral a => a -> [Ratio a]
rationalSumLowest n = [ i % j | i <- [1..n], let j = n-i, i+j == n, gcd i j == 1, j /=0]

rationals :: [Ratio Integer]
rationals =  concat [rationalSumLowest i | i <- [1..]]

sumFile :: IO ()
sumFile = do
	let input = "input.txt"
	allText <- readFile input
	let listedText = splitAtSeparator '\n' allText
	let sumList = addAllNumbersInList listedText
	print sumList
	
addAllNumbersInList [] = 0
addAllNumbersInList (x:xs) =  (read x) + addAllNumbersInList xs

splitAtSeparator :: Eq a => a -> [a] -> [[a]]
splitAtSeparator sep [] = []
splitAtSeparator sep content = first : splitAtSeparator sep rest
    where
    first = takeWhile (/= sep) content
    firstlen = length first
    rest = drop (firstlen+1) content