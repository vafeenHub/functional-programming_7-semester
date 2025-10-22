-- 2. переместить геомутрическую фигуру на определенный вектор.
-- Входные параметры: фигура и вектор

-- Определение типа Point для представления точки
data Point = Point Double Double deriving (Show)

-- Определение типа Geometry для представления геометрических фигур
data Geometry = Circle Point Point 
              | Rectangle Point Double Double 
              deriving (Show)

-- Функция для вычисления расстояния между двумя точками
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Функция для вычисления площади геометрической фигуры
area :: Geometry -> Double
area (Circle center radiusPoint) = 
    let r = distance center radiusPoint
    in pi * r * r
area (Rectangle _ width height) = width * height

-- Функция для перемещения точки на заданный вектор
movePoint :: Point -> Point -> Point
movePoint (Point x1 y1) (Point dx dy) = Point (x1 + dx) (y1 + dy)

-- Функция для перемещения геометрической фигуры на заданный вектор
moveGeometry :: Geometry -> Point -> Geometry
moveGeometry (Circle center radiusPoint) vector =
    Circle (movePoint center vector) (movePoint radiusPoint vector)
moveGeometry (Rectangle topLeft width height) vector =
    Rectangle (movePoint topLeft vector) width height

-- Примеры использования
main :: IO ()
main = do
    let center = Point 0 0
        radiusPoint = Point 5 0
        circle = Circle center radiusPoint
    
    let rect = Rectangle (Point 2 2) 4 3
    
    let moveVector = Point 3 1
    
    putStrLn "Исходные фигуры:"
    putStrLn $ "Круг: " ++ show circle
    putStrLn $ "Прямоугольник: " ++ show rect
    
    putStrLn "\nПлощади фигур:"
    putStrLn $ "Площадь круга: " ++ show (area circle)
    putStrLn $ "Площадь прямоугольника: " ++ show (area rect)
    
    putStrLn $ "\nПеремещение на вектор: " ++ show moveVector
    let movedCircle = moveGeometry circle moveVector
    let movedRect = moveGeometry rect moveVector
    
    putStrLn "Перемещенные фигуры:"
    putStrLn $ "Круг: " ++ show movedCircle
    putStrLn $ "Прямоугольник: " ++ show movedRect
    
    putStrLn "\nПлощади перемещенных фигур:"
    putStrLn $ "Площадь круга: " ++ show (area movedCircle)
    putStrLn $ "Площадь прямоугольника: " ++ show (area movedRect)