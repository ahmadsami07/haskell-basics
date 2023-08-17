-- P1
myLast [x] = x
myLast (_:xs) = myLast xs
--P2
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs
-- P3
elementAt (xs) n = xs !! n
-- P4
myLength [] = 0
myLength (_:xs) = 1+ myLength (xs)