doubleMe x = x + x
doubleUs x y = (x*2) + (y*2)
doubleSmallNumber x = if x < 100
    then x*2
    else x

safeHead alist = if null alist 
    then [] 
    else [head alist]

factorial n = product [1..n]
flatten xxs = [ x | xs <- xxs, x <- xs]

triangles = [
        (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], 
        a^2 + b^2 == c^2, 
        a + b + c == 24
    ]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

checkIsSeven :: (Integral a) => a -> String 
checkIsSeven 7 = "Woohoo, seven, yay"
checkIsSeven x = "Not seven.. :("

factorialRec :: (Integral a) => a -> a
factorialRec 0 = 1
factorialRec n = factorialRec (n-1) * n

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

myHead :: [a] -> a
myHead [] = error "Can't compute head of empty list!"
myHead (x:_) = x

length' :: [a] -> Int 
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital all@(x:_) = "The first letter of '" ++ all ++ "' is '" ++ [x] ++ "'"
capital [] = "Supply non-empty string"

max' :: (Ord a) => a -> a -> a
max' x y
    | x >= y    = x
    | otherwise = y

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell height weight
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25 = "Normal"
    | otherwise = "Overweight"
    where bmi = weight / (height / 100)^2


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi h w | (h, w) <- xs, let bmi h w = w / (h / 100)^2]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Real a) => Int -> a -> [a]
replicate' qty num
    | qty <= 0 = []
    | otherwise = num : replicate' (qty-1) num

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [a] -> [(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
