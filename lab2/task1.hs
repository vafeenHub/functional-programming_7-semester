-- 1. Придумать свой класс типов, который содержит как минимум две функции, одна из которых выражается через другие. 
-- Написать реализацию этого класса типов для любых двух типов данных, типы данных выбирать такие, чтобы их реализации отличались (можно использовать свои собственные типы данных). 

class Incrmentable a where
  incrBy :: Int-> a->a
  incr :: a->a
  incr = incrBy 1

-- Тут newtype потому что одно поле и не нужно больше
newtype Counter = Counter Int
  deriving (Show, Eq)

instance Incrmentable Counter where
  incrBy n (Counter x) = Counter (x + n)
  -- incr использует реализацию по умолчанию

-- Тип 2: "часы" по модулю 12 (от 1 до 12)
newtype ClockTime = ClockTime Int
  deriving (Show, Eq)

normalize :: Int->Int
normalize h = ((h - 1) `mod` 12)+1  -- 1..12

instance Incrmentable ClockTime where
  incrBy n (ClockTime h) = ClockTime (normalize (h + n))
  -- incr снова использует реализацию по умолчанию,
  -- но incrBy реализована иначе → поведение отличается

-- Примеры использования:
main :: IO ()
main = do
  print $ incr (Counter 5)               -- Counter 6
  print $ incrBy 3 (Counter 5)           -- Counter 8
  print $ incr (ClockTime 11)            -- ClockTime 12
  print $ incr (ClockTime 12)            -- ClockTime 1
  print $ incrBy 5 (ClockTime 10)        -- ClockTime 3