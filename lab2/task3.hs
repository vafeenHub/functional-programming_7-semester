-- 13.  Создать тип данных геометрическая фигура с конструкторами, как минимум, для квадрата, треугольника, прямоугольника и круга. Написать функции нахождения площади и периметра для фигуры. Написать функцию, которая умеет превращать заданную фигуру в другую с сохранением периметра и центра фигуры (например из квадрата в круг), функция будет принимать в качестве параметров исходную  фигуру и тип фигуры в какую нужно превратить.  

-- Тип данных для геометрической фигуры
-- Каждая фигура хранит координаты центра (x, y) и параметры
data Shape
  = Square    Double Double Double    -- x, y, a
  | Rectangle Double Double Double Double -- x, y, a, b
  | Triangle  Double Double Double    -- x, y, a
  | Circle    Double Double Double    -- x, y, r
  deriving (Show, Eq)


perimeter :: Shape -> Double
perimeter (Square _ _ a)       = 4 * a
perimeter (Rectangle _ _ a b)  = 2 * (a + b)
perimeter (Triangle _ _ a)  = 3 * a
perimeter (Circle _ _ r)       = 2 * pi * r

area :: Shape -> Double
area (Square _ _ a) = a * a
area (Rectangle _ _ w h) = w * h
area (Triangle _ _ a) = (sqrt 3 / 4) * a * a
area (Circle _ _ r) = pi * r * r

-- Функция преобразования фигуры в другую с сохранением периметра и центра
convertTo :: Shape -> Shape -> Shape
convertTo original target = case target of
  Square x y _      -> makeSquare cx cy p
  Rectangle x y _ _ -> makeRectangle cx cy p
  Triangle x y _    -> makeTriangle cx cy p
  Circle x y _      -> makeCircle cx cy p
  where
    p = perimeter original
    (cx, cy) = getCenter original

-- Вспомогательные функции для создания фигур с заданным периметром

makeSquare :: Double -> Double -> Double -> Shape
makeSquare x y p = Square x y (p / 4)

makeRectangle :: Double -> Double -> Double -> Shape
makeRectangle x y p =
  -- Квадрат как частный случай прямоугольника
  let a = p / 4
  in Rectangle x y a a

makeTriangle :: Double -> Double -> Double -> Shape
makeTriangle x y p =
  -- Равносторонний треугольник
  Triangle x y (p / 3)

makeCircle :: Double -> Double -> Double -> Shape
makeCircle x y p = Circle x y (p / (2 * pi))

-- Функция для извлечения центра фигуры
getCenter :: Shape -> (Double, Double)
getCenter (Square x y _)       = (x, y)
getCenter (Rectangle x y _ _)  = (x, y)
getCenter (Triangle x y _)     = (x, y)
getCenter (Circle x y _)       = (x, y)

main :: IO ()
main = do
  -- Исходный квадрат
  let square = Square 0 0 4
  
  putStrLn "Исходный квадрат:"
  print square
  putStrLn $ "Периметр: " ++ show (perimeter square)
  putStrLn $ "Площадь: " ++ show (area square)
  
  -- Преобразуем в круг
  let circle = convertTo square (Circle 0 0 1)
  
  putStrLn "\nПреобразовано в круг:"
  print circle
  putStrLn $ "Периметр: " ++ show (perimeter circle)
  putStrLn $ "Радиус: " ++ getRadius circle
  
  -- Преобразуем в треугольник
  let triangle = convertTo square (Triangle 0 0 1)
  
  putStrLn "\nПреобразовано в треугольник:"
  print triangle
  putStrLn $ "Периметр: " ++ show (perimeter triangle)
  putStrLn $ "Сторона: " ++ getTriangleSide triangle

-- Вспомогательные функции для получения параметров
getRadius :: Shape -> String
getRadius (Circle _ _ r) = show r
getRadius _ = "не круг"

getTriangleSide :: Shape -> String
getTriangleSide (Triangle _ _ a) = show a
getTriangleSide _ = "не треугольник"