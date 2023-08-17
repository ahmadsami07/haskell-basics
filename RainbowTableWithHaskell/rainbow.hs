import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe as Maybe

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 5
nLetters = 18
width = 60
height = 800
filename = "table.txt"  -- filename to store the table


pwReduce:: Hash -> String
pwReduce x = baseConvert [] x 

baseConvert :: [Hash] -> Hash  -> [Char]
baseConvert xs x 
 | length(xs)==pwLength = map toLetter(reverse (map fromIntegral xs))
 | otherwise = baseConvert remainderList quotient
 where remainderList = xs ++ [x `mod` fromIntegral nLetters]
       quotient  = x `div` fromIntegral nLetters


-- Building the table


rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable x xs = produceRainbowTable x xs xs 0

--xy is the original list of passwords to be 

produceRainbowTable :: Int -> [Passwd] -> [Passwd] -> Int -> Map.Map Hash Passwd
produceRainbowTable x xs xy y
 | y /= (x) = produceRainbowTable x reducedList xy (y+1)
 | otherwise = Map.fromList (tupledList)
 where reducedList = map pwReduce (map pwHash xs)
       tupledList= createTupledList finalHash xy []
       finalHash = map pwHash xs

createTupledList :: [a1] -> [a2] -> [(a1, a2)] -> [(a1, a2)]
createTupledList [] [] a = a
createTupledList (_:_) [] a = a
createTupledList [] (_:_) a = a
createTupledList (x:xs) (y:xy) a =  createTupledList xs xy addTuple
 where addTuple = a++ [(x,y)]

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table newWidth hash = findCorrectChain table newWidth hash hash newWidth

findCorrectChain :: Map.Map Hash Passwd -> Int -> Hash -> Hash -> Int -> Maybe Passwd
findCorrectChain table newWidth hash originalHash originalWidth
 | newWidth == -1 = Nothing
 | hashLookup == Nothing = findCorrectChain table (newWidth-1) newHash originalHash originalWidth
 | checkIfCorrectChainForCollisionDetection hashLookup newWidth originalHash  == True = findPasswordFromChain hashLookup originalWidth originalHash
 | checkIfCorrectChainForCollisionDetection hashLookup newWidth originalHash  == False = findCorrectChain table (newWidth-1) newHash originalHash originalWidth
 | otherwise = Nothing
 where hashLookup = Map.lookup hash table 
       newHash = pwHash (pwReduce hash)


checkIfCorrectChainForCollisionDetection :: Maybe Passwd -> Int -> Hash -> Bool
checkIfCorrectChainForCollisionDetection Nothing _ _ = False
checkIfCorrectChainForCollisionDetection (Just passwd) newWidth originalHash
 | newWidth == -1 = False
 | pwHash(passwd) == originalHash =  True
 | otherwise = checkIfCorrectChainForCollisionDetection reducedPass (newWidth-1) originalHash
  where reducedPass = Just(pwReduce(pwHash passwd))

findPasswordFromChain :: Maybe Passwd -> Int -> Hash -> Maybe Passwd
findPasswordFromChain Nothing _ _ = Nothing
findPasswordFromChain (Just passwd) newWidth originalHash
 | newWidth == -1 = Nothing
 | pwHash(passwd) == originalHash =  Just passwd
 | otherwise = findPasswordFromChain reducedPass (newWidth-1) originalHash
  where reducedPass = Just(pwReduce(pwHash passwd))

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)

main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res