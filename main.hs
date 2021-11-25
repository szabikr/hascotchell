fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

fib :: Int -> [Int]
fib 0 = [0]
fib n = fib (n - 1) ++ [fib' n]

sum' :: [Int] -> Int
sum' (x:y) = x + sum' y
sum' _ = 0

multiplyBy2 :: [Int] -> [Int]
multiplyBy2 [] = []
multiplyBy2 x = [(head x * 2)] ++ (multiplyBy2 $ tail x)

multiplyBy2' :: [Int] -> [Int]
multiplyBy2' (x:y) = [x * 2] ++ multiplyBy2' y
multiplyBy2' _ = []

main = putStrLn $ show $ multiplyBy2' (fib 20)
