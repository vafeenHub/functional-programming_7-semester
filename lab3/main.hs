-- 3) 21, 26, 52, 53, 59, 2.2 

-- 52. Найти все целые числа из промежутка от a до b, у которых количество делителей равно k

-- Функция подсчёта количества делителей числа n
countDivisors :: Int -> Int
countDivisors n = length [d | d <- [1..abs n], n `mod` d == 0]

-- Функция поиска чисел из диапазона [a..b] с количеством делителей k
findNumbersWithKDivisors :: Int -> Int -> Int -> [Int]
findNumbersWithKDivisors a b k = 
    [x | x <- [a..b], countDivisors x == k]

main :: IO ()
main = do
    let a = 1
    let b = 30
    let k = 4
    let result = findNumbersWithKDivisors a b k
    putStrLn $ "Числа из диапазона [" ++ show a ++ ", " ++ show b ++ "] c " ++ show k ++ " делителями: "
    print result
