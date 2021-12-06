module Main where

elements = [1..20]



myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = Just (head $ reverse xs)

myLast' :: [a] -> Maybe a
myLast' [] = Nothing
myLast' [x] = Just x
myLast' (x:xs) = myLast' xs


myButLast :: [a] -> a
myButLast [] = error "list is empty"
myButLast [x] = error "list has only one element"
myButLast (x:[xs]) = x
myButLast (x:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' [] = error "list is empty"
myButLast' [x] = error "list has only one element"
myButLast' xs = last $ init xs

myButLast'' :: [a] -> a
myButLast'' [] = error "list is empty"
myButLast'' [x] = error "list has only one element"
myButLast'' (x:[xs]) = x
myButLast'' xs = myButLast'' $ tail xs


elementAt :: [a] -> Int -> a
elementAt [] _ = error "list is empty"
elementAt xs 0 = head xs
elementAt (x:xs) n 
    | n < 0 || n >= length xs = error "index invalid"
    | otherwise = elementAt xs (n - 1)


myLength :: [a] -> Int
myLength [] = 0
myLength x = 1 + myLength (tail x)

myLength' :: [a] -> Int
myLength' [] = 0
myLength' (_:xs) = 1 + myLength' xs

myLength'' :: [a] -> Int 
myLength'' list = myLength_acc list 0
    where
        myLength_acc [] n = n
        myLength_acc (_:xs) n = myLength_acc xs (n + 1)


myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

myReverse'' :: [a] -> [a]
myReverse'' list = myReverse_acc list []
    where 
        myReverse_acc [] reversed = reversed
        myReverse_acc (x:xs) reversed = myReverse_acc xs (x:reversed)

isPalindrom :: [a] -> Bool
isPalindrom xs = xs == myReverse' xs


-- questions
-- what are 'foldr' and 'foldl'

main :: IO ()
main = case myLast' [1, 2, 3, 4, 5, 6, 7, 8, 9] of
    Just a -> print a
    Nothing -> print "Nothing"
