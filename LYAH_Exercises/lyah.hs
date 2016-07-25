doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumberx x = (if x > 100 then x else x*2) + 1
doubleSmallNumber x = if x > 100
  then x
  else x*2
lostNumbers = [3,7,20,38,39,40]
boomBangs xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x]
-- length' xs = sum [1 | _ <- xs]
removeNonUp st = [ c | c <- st, c `elem` ['A'..'Z']]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
-- [[x | x <- xs, even x] | xs <- xxs]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.0 = "you're thin"
  | bmi <= 25.0 = "you're average"
  | bmi <= 30.0 = "you're fat"
  | otherwise   = "you're super fat"

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let side = 2 * pi * r * h
      topbottom = 2 * pi * r^2
  in side + topbottom

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "Empty list"
                       (x:_) -> x

-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ case xs of [] -> "Empty"
--                                               [x] -> "Singleton"
--                                               xs -> "At least two"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

myFun :: (Integral a) => a
myFun = head( filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- myFunc = sum [n^2 | n <- [10000,9999..], odd (n^2)]

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | odd n  = n: chain (3*n + 1)
  | even n = n: chain (n `div` 2)


numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- listOfFuns = map (*) [1,2..100]

--elem'' :: (Eq a) => a -> [a] -> Bool
--elem'' y ys = foldl (\acc x -> if x ==y then True else acc)False ys

--sum''' :: (Num a) => [a] -> a
--sum''' = foldl (+) 0

--map'' :: (a -> b) -> [a] -> [b]
--map'' f xs = foldr (\x acc -> f x : acc) [] xs

--maximumn :: (Ord a) => [a] -> a
--maximumn = foldr1 (\x acc -> if x > acc then x else acc)

--reversen :: [a] -> [a]
--reversen = foldr1 (\x acc -> x : acc) []

--productn :: (Num a) => [a] -> a
--productn = foldr1 (*)

-- filtern :: (a -> Bool) => [a] -> [a]
-- filtern f = foldr (\x acc -> if f x then x : acc else acc) []

--headn :: [a] -> a
--headn = foldr1 (\x _ -> x)

-- lastn :: [a] -> a
-- lastn = foldl (\_ x -> x)

-- How many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000?
-- length ( takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum' :: Integer
oddSquareSum' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

myLast :: [a] -> a
myLast xs = head (reverse xs)

myLastButOne :: [a] -> a
myLastButOne xs = xs !! ((length xs) - 2)

elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

--findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
--findKey [] = Nothing
--findKey ((k,v):xs) = if key == k
--                        then Just v
--                        else findKey key xs

--findKey key = foldr (\(k,v) acc) -> if key == k then Just v else acc) Nothing

--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
