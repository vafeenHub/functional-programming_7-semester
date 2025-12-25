-- 1. Посчитать минимальное расстояние до центра координат, учитывая что стороны прямоугольника параллельны осям координат
-- 2. Добавить квадрат и треугольник к типам данных и дописать для них ранее написанные функции

{-# LANGUAGE DeriveDataTypeable #-}

data Point = Point Double Double deriving (Show, Eq)

data Shape = 
    Rectangle Point Point      -- левый нижний и правый верхний углы
  | Square Point Double        -- левый нижний угол и длина стороны
  | Triangle Point Point Point -- три вершины
  deriving (Show, Eq)


--  расстояние от точки до центра координат
distanceToPoint :: Point -> Double
distanceToPoint (Point x y) = sqrt (x*x + y*y)

--  минимальное расстояние среди списка точек
minDistanceFromList :: [Point] -> Double
minDistanceFromList points = minimum (map distanceToPoint points)

-- вершины прямоугольника
rectanglePoints :: Point -> Point -> [Point]
rectanglePoints (Point x1 y1) (Point x2 y2) =
  [Point x1 y1, Point x1 y2, Point x2 y1, Point x2 y2]

-- вершины квадра
squarePoints :: Point -> Double -> [Point]
squarePoints (Point x y) side =
  [Point x y, 
   Point (x + side) y,
   Point x (y + side),
   Point (x + side) (y + side)]

-- Для треугольника: все три вершины
trianglePoints :: Point -> Point -> Point -> [Point]
trianglePoints p1 p2 p3 = [p1, p2, p3]

-- Основная функция для вычисления минимального расстояния до центра координат
mindistanceToPoint :: Shape -> Double
mindistanceToPoint shape = case shape of
  Rectangle p1 p2 -> minDistanceFromList (rectanglePoints p1 p2)
  Square p side   -> minDistanceFromList (squarePoints p side)
  Triangle p1 p2 p3 -> minDistanceFromList (trianglePoints p1 p2 p3)

main :: IO ()
main = do
  let rect = Rectangle (Point 1 1) (Point 4 5)
      sq = Square (Point 2 2) 3
      tri = Triangle (Point 0 0) (Point 4 0) (Point 2 3)
  
  putStrLn $ "Прямоугольник: " ++ show rect
  putStrLn $ "Квадрат: " ++ show sq
  putStrLn $ "Треугольник: " ++ show tri
  
  putStrLn "\nМинимальное расстояние до центра координат:"
  putStrLn $ "Прямоугольник: " ++ show (mindistanceToPoint rect)
  putStrLn $ "Квадрат: " ++ show (mindistanceToPoint sq)
  putStrLn $ "Треугольник: " ++ show (mindistanceToPoint tri)