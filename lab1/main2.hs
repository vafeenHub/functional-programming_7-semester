-- Написать функцию находящую сумму ряда с заданной точностью 
-- ℇ. Вычисление заканчивается, если модуль очередного слагаемого 
-- становится меньше заданного значения точности ℇ. Проверять 
-- корректность можно по контрольной формуле.

-- | Вычисление факториала натурального числа n
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | Вычисление i-го слагаемого ряда
-- i - номер слагаемого (начинается с 1)
-- x - параметр в ряде
term :: Double -> Int -> Double
term x i =
  let
    n = 2 * toInteger i               -- знаменатель факториал вычисляем для 2i
    numerator = (x ** fromIntegral (2*i - 1)) * (fromIntegral (2*i) + x)
    denominator = fromIntegral (factorial n)
    sign = if even i then -1 else 1   -- знак зависит от чётности i: нечет i -> +
  in sign * numerator / denominator

-- | Суммирование ряда с точностью epsilon
-- x - параметр ряда
-- eps - заданная точность (модуль члена меньше eps - остановка)
sumSeries :: Double -> Double -> Double
sumSeries x eps = sumHelper 1 0  -- начинаем с i=1 и начальной суммы 0
  where
    -- sumHelper выполняет рекурсивное суммирование ряда
    -- i - номер текущего слагаемого
    -- acc - накопленная сумма
    sumHelper i acc =
      let currentTerm = term x i      -- вычисляем i-е слагаемое
      in if abs currentTerm < eps     -- если модуль слагаемого меньше точности,
         then acc                     -- возвращаем накопленную сумму
         else sumHelper (i + 1) (acc + currentTerm) -- иначе добавляем слагаемое и повторяем для следующего i

-- | Контрольная формула: sin x - cos x + 1
checkFormula :: Double -> Double
checkFormula x = sin x - cos x + 1

-- | Пример использования
main :: IO ()
main = do
  let x = 1.0
  let eps = 1e-6
  let approx = sumSeries x eps
  let exact = checkFormula x
  putStrLn $ "При x = " ++ show x ++ " и точности ε = " ++ show eps
  putStrLn $ "Сумма ряда (приближенно): " ++ show approx
  putStrLn $ "Контрольная формула:       " ++ show exact
  putStrLn $ "Погрешность:              " ++ show (abs (approx - exact))
