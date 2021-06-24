-- ==================================================
-- Q1
-- | import Data.Char
import Data.Char

type Text = [Char]
type Wrd = [Char]


sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
 where (ys,zs) = half xs
half xs = (take n xs, drop n xs)
 where n = length xs `div` 2
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

sortWords :: [Wrd] -> [Wrd]
sortWords = sort
sortRuns :: [(Int,Wrd)] -> [(Int,Wrd)]
sortRuns = reverse . sort

showRun :: (Int,Wrd) -> String
showRun (n,w) = w ++ ": " ++
 show n ++ "\n"
 
spann :: (a -> Bool) -> [a] -> ([a], [a])
spann p [] = ([], [])
spann p (x:xs) = if p x then (x:ys,zs)
 else ([],x:xs)
 where (ys,zs) = spann p xs

countRuns :: [Wrd] -> [(Int,Wrd)]
countRuns [] = []
countRuns (w:ws) = (1+length us,w) : countRuns vs
 where (us,vs) = spann ( == w) ws


commonWords :: Int -> Text -> String
commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords . map (map Data.Char.toLower) . words

-- commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map Data.Char.toLower

-- ==========================================
-- Q2

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) | n > 0 = x : myTake (n - 1) xs
myTake _ _ = error "take: negative argument"

myDrop :: Int -> [a] -> [a]
myDrop 0 (x:xs) = (x:xs)
myDrop _ [] = []
myDrop n (_:xs) | n > 0 = myDrop (n-1) xs
myDrop _ _ = error "drop :negative argument"


-- =============================================
-- Q3
data List a = Nil | Snoc (List a) a deriving (Show)
scar :: List a -> a
scar Nil = error "Empty"
scar (Snoc Nil x) = x
scar (Snoc l x) = scar l


scdr :: List a -> List a
scdr Nil = error "Empty"
scdr (Snoc Nil x) = Nil
scdr (Snoc l x) = Snoc (scdr l) x 

toList :: [a] -> List a
toList [] = Nil
toList l = Snoc (toList (init l)) (last l)

fromList :: List a -> [a]
fromList Nil = []
fromList l = ((scar l) : fromList (scdr l))
