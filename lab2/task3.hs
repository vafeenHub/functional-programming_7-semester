-- 13.  Создать тип данных геометрическая фигура с конструкторами, как минимум, для квадрата, треугольника, прямоугольника и круга. Написать функции нахождения площади и периметра для фигуры. Написать функцию, которая умеет превращать заданную фигуру в другую с сохранением периметра и центра фигуры (например из квадрата в круг), функция будет принимать в качестве параметров исходную  фигуру и тип фигуры в какую нужно превратить.  

-- Тип данных для геометрической фигуры
-- Каждая фигура хранит координаты центра (x, y) и параметры
data Shape
  = Square    Double Double Double    -- x, y, a
  | Rectangle Double Double Double Double -- x, y, a, b
  | Triangle  Double Double Double Double Double -- x, y, a, b, c
  | Circle    Double Double Double    -- x, y, r
  deriving (Show, Eq)


perimeter :: Shape -> Double
perimeter (Square _ _ a)       = 4 * a
perimeter (Rectangle _ _ a b)  = 2 * (a + b)
perimeter (Triangle _ _ a b c) = a + b + c
perimeter (Circle _ _ r)       = 2 * pi * r

area :: Shape -> Double
area (Square _ _ a) = a * a
area (Rectangle _ _ w h) = w * h
area (Triangle _ _ a b c) =
  -- Формула Герона
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))
area (Circle _ _ r) = pi * r * r

-- Функция преобразования фигуры в другую с сохранением периметра и центра
convertTo :: Shape -> Shape -> Shape
convertTo original target = case target of
  Square x y _       -> makeSquare x y p
  Rectangle x y _ _  -> makeRectangle x y p
  Triangle x y _ _ _ -> makeTriangle x y p
  Circle x y _       -> makeCircle x y p
  where
    p = perimeter original
    (cx, cy) = getCenter original

-- Вспомогательные функции для создания фигур с заданным периметром

makeSquare :: Double -> Double -> Double -> Shape
makeSquare x y p = Square x y (p / 4)

makeRectangle :: Double -> Double -> Double -> Shape
makeRectangle x y p =
  -- Предположим, что ширина = высоте * 2 (или можно выбрать квадрат как частный случай)
  -- Но чтобы однозначно определить прямоугольник по периметру — нужна дополнительная информация.
  -- Поэтому сделаем квадрат как частный случай прямоугольника (w = h)
  let side = p / 4
  in Rectangle x y side side

makeTriangle :: Double -> Double -> Double -> Shape
makeTriangle x y p =
  -- Сделаем равносторонний треугольник (единственный способ однозначно определить по периметру)
  let side = p / 3
  in Triangle x y side side side

makeCircle :: Double -> Double -> Double -> Shape
makeCircle x y p = Circle x y (p / (2 * pi))

-- Функция для извлечения центра фигуры
getCenter :: Shape -> (Double, Double)
getCenter (Square x y _)       = (x, y)
getCenter (Rectangle x y _ _)  = (x, y)
getCenter (Triangle x y _ _ _) = (x, y)
getCenter (Circle x y _)       = (x, y)


main :: IO ()
main = do
  -- Исходная фигура: квадрат в центре (0,0), сторона = 4
  let originalSquare = Square 0 0 4

  putStrLn "=== Исходная фигура: квадрат ==="
  print originalSquare
  putStrLn $ "Периметр: " ++ show (perimeter originalSquare)
  putStrLn $ "Площадь: " ++ show (area originalSquare)

  -- Преобразуем в круг (сохраняя периметр и центр)
  let circleFromSquare = convertTo originalSquare (Circle 0 0 1) -- 1 — заглушка

  putStrLn "\n=== Преобразовано в круг ==="
  print circleFromSquare
  putStrLn $ "Периметр: " ++ show (perimeter circleFromSquare)
  putStrLn $ "Площадь: " ++ show (area circleFromSquare)

  -- Преобразуем в равносторонний треугольник
  let triangleFromSquare = convertTo originalSquare (Triangle 0 0 1 1 1)

  putStrLn "\n=== Преобразовано в треугольник ==="
  print triangleFromSquare
  putStrLn $ "Периметр: " ++ show (perimeter triangleFromSquare)
  putStrLn $ "Площадь: " ++ show (area triangleFromSquare)
