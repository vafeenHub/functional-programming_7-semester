import System.Process (system)
import Data.IORef

-- Тип данных для геометрической фигуры
data Shape
  = Square    Double Double Double    -- x, y, a
  | Rectangle Double Double Double Double -- x, y, a, b
  | Triangle  Double Double Double    -- x, y, a
  | Circle    Double Double Double    -- x, y, r
  deriving (Show, Eq)

perimeter :: Shape -> Double
perimeter (Square _ _ a)       = 4 * a
perimeter (Rectangle _ _ a b)  = 2 * (a + b)
perimeter (Triangle _ _ a)     = 3 * a
perimeter (Circle _ _ r)       = 2 * pi * r

area :: Shape -> Double
area (Square _ _ a)      = a * a
area (Rectangle _ _ w h) = w * h
area (Triangle _ _ a)    = (sqrt 3 / 4) * a * a
area (Circle _ _ r)      = pi * r * r

getCenter :: Shape -> (Double, Double)
getCenter (Square x y _)       = (x, y)
getCenter (Rectangle x y _ _)  = (x, y)
getCenter (Triangle x y _)     = (x, y)
getCenter (Circle x y _)       = (x, y)

makeSquare :: Double -> Double -> Double -> Shape
makeSquare x y p = Square x y (p / 4)

makeRectangle :: Double -> Double -> Double -> Shape
makeRectangle x y p = let a = p / 4 in Rectangle x y a a

makeTriangle :: Double -> Double -> Double -> Shape
makeTriangle x y p = Triangle x y (p / 3)

makeCircle :: Double -> Double -> Double -> Shape
makeCircle x y p = Circle x y (p / (2 * pi))

convertTo :: Shape -> Shape -> Shape
convertTo original target = case target of
  Square _ _ _      -> makeSquare cx cy p
  Rectangle _ _ _ _ -> makeRectangle cx cy p
  Triangle _ _ _    -> makeTriangle cx cy p
  Circle _ _ _      -> makeCircle cx cy p
  where
    p = perimeter original
    (cx, cy) = getCenter original

-- Проверка принадлежности точки фигуре
inside :: Shape -> Double -> Double -> Bool
inside (Square cx cy a) px py =
  let half = a / 2 in abs (px - cx) <= half && abs (py - cy) <= half -- то есть расстояние от любой точки не больше половины
inside (Rectangle cx cy w h) px py =
  let hw = w / 2; hh = h / 2 in abs (px - cx) <= hw && abs (py - cy) <= hh -- то есть расстояние по высоте половина высоты, по ширине половина ширины
  -- Сторона: a
-- Высота: h = a * корень(3) / 2 
-- Центр тяжести (точка пересечения медиан) делит высоту в соотношении 2:1:
-- от вершины до центра — 2/3 высоты
-- от центра до основания — 1/3 высоты
-- =>
-- topY = cy + (2/3) * height — координата верхней вершины
-- baseY = cy - (1/3) * height — уровень основания

inside (Triangle cx cy a) px py =
  let height = a * sqrt 3 / 2
      topY = cy + (2/3) * height
      baseY = cy - (1/3) * height
  in py >= baseY && py <= topY &&
     let ratio = (topY - py) / (topY - baseY) -- расстояние от низа до точки / расстояние от низа до верха 
         currentHalf = (a / 2) * ratio -- и это поулчается сторона * 0.5 и умножить на текущую дробь на которую мы поднялись
     in abs (px - cx) <= currentHalf
inside (Circle cx cy r) px py =
  let dx = px - cx; dy = py - cy in dx*dx + dy*dy <= r*r

-- ASCII-рендер с координатными осями
renderShapeWithAxes :: Shape -> IO ()
renderShapeWithAxes shape = do
  putStrLn "\nASCII-визуализация с осями (диапазон: x,y ∈ [-10,10]):"
  mapM_ (renderLine shape) [10,9..(-10)]
  where
    renderLine s y = do
      let chars = [ renderChar s x y | x <- [-10..10] ]
      putStrLn chars
    renderChar s x y =
      let px = fromIntegral x
          py = fromIntegral y
          onXAxis = (y == 0)
          onYAxis = (x == 0)
          atOrigin = (x == 0 && y == 0)
          inShape = inside s px py
      in
        if atOrigin
          then if inShape then '*' else '+'
          else if inShape
            then '*'
            else if onXAxis then '-'
            else if onYAxis then '|'
            else ' '

-- Очистка экрана (Linux/macOS)
clearScreen :: IO ()
clearScreen = system "clear" >> return ()

-- Безопасный ввод числа
readDouble :: String -> IO Double
readDouble prompt = do
  putStr prompt
  line <- getLine
  case reads line of
    [(x, "")] -> return x
    _         -> do
      putStrLn "Некорректное число. Попробуйте снова."
      readDouble prompt

-- Ввод новой фигуры
readShape :: IO Shape
readShape = do
  putStrLn "\nВыберите тип фигуры:"
  putStrLn "1. Квадрат"
  putStrLn "2. Прямоугольник"
  putStrLn "3. Равносторонний треугольник"
  putStrLn "4. Круг"
  choice <- getLine
  cx <- readDouble "Введите x центра: "
  cy <- readDouble "Введите y центра: "
  case choice of
    "1" -> do
      a <- readDouble "Введите длину стороны: "
      return $ Square cx cy a
    "2" -> do
      w <- readDouble "Введите ширину (a): "
      h <- readDouble "Введите высоту (b): "
      return $ Rectangle cx cy w h
    "3" -> do
      a <- readDouble "Введите длину стороны: "
      return $ Triangle cx cy a
    "4" -> do
      r <- readDouble "Введите радиус: "
      return $ Circle cx cy r
    _ -> do
      putStrLn "Неверный выбор. Создаём квадрат."
      a <- readDouble "Введите длину стороны: "
      return $ Square cx cy a

-- Ввод целевого типа (для преобразования)
readTargetType :: IO Shape
readTargetType = do
  putStrLn "\nПреобразовать в:"
  putStrLn "1. Квадрат"
  putStrLn "2. Прямоугольник"
  putStrLn "3. Треугольник"
  putStrLn "4. Круг"
  choice <- getLine
  case choice of
    "1" -> return (Square 0 0 1)
    "2" -> return (Rectangle 0 0 1 1)
    "3" -> return (Triangle 0 0 1)
    "4" -> return (Circle 0 0 1)
    _ -> return (Square 0 0 1)

-- Показать все сохранённые фигуры
showSavedShapes :: [Shape] -> IO ()
showSavedShapes [] = putStrLn "Нет сохранённых фигур."
showSavedShapes shapes = do
  putStrLn "\nСохранённые фигуры:"
  mapM_ (\(i, sh) -> putStrLn $ show i ++ ". " ++ show sh) (zip [1..] shapes)

-- Выбор индекса для удаления
selectIndex :: [Shape] -> IO (Maybe Int)
selectIndex [] = return Nothing
selectIndex shapes = do
  showSavedShapes shapes
  putStr "Введите номер фигуры для удаления (или 0 для отмены): "
  line <- getLine
  case reads line of
    [(n, "")] | n >= 1 && n <= length shapes -> return (Just (n - 1))
               | otherwise                    -> return Nothing
    _ -> return Nothing

-- Главный цикл
main :: IO ()
main = do
  shapesRef <- newIORef []
  putStrLn "Добро пожаловать! Геометрические фигуры (лаб. №3)."
  putStrLn "Нажмите Enter для продолжения..."
  _ <- getLine
  menuLoop shapesRef

menuLoop :: IORef [Shape] -> IO ()
menuLoop shapesRef = do
  clearScreen
  putStrLn "\n=== Меню ==="
  putStrLn "1. Создать и сохранить новую фигуру"
  putStrLn "2. Показать все сохранённые фигуры"
  putStrLn "3. Преобразовать фигуру и сохранить результат"
  putStrLn "4. Визуализировать фигуру (с осями координат)"
  putStrLn "5. Удалить фигуру"
  putStrLn "6. Выйти"
  putStr "Ваш выбор: "
  choice <- getLine
  case choice of
    "1" -> do
      shape <- readShape
      modifyIORef shapesRef (shape :)
      putStrLn "\n✅ Фигура успешно сохранена."
      putStrLn "\nНажмите Enter для возврата в меню..."
      _ <- getLine
      menuLoop shapesRef

    "2" -> do
      shapes <- readIORef shapesRef
      showSavedShapes shapes
      putStrLn "\nНажмите Enter для возврата в меню..."
      _ <- getLine
      menuLoop shapesRef

    "3" -> do
      shapes <- readIORef shapesRef
      if null shapes
        then do
          putStrLn "❌ Нет фигур для преобразования."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef
        else do
          showSavedShapes shapes
          putStr "Выберите номер исходной фигуры: "
          line <- getLine
          case reads line of
            [(n, "")] | n >= 1 && n <= length shapes -> do
              let orig = shapes !! (n-1)
              targetTemplate <- readTargetType
              let converted = convertTo orig targetTemplate
              modifyIORef shapesRef (converted :)
              putStrLn "\n✅ Преобразованная фигура сохранена:"
              print converted
              putStrLn $ "Периметр: " ++ show (perimeter converted)
              putStrLn $ "Площадь: " ++ show (area converted)
            _ -> putStrLn "❌ Неверный номер."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef

    "4" -> do
      shapes <- readIORef shapesRef
      if null shapes
        then do
          putStrLn "❌ Нет фигур для визуализации."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef
        else do
          showSavedShapes shapes
          putStr "Выберите номер фигуры для визуализации: "
          line <- getLine
          case reads line of
            [(n, "")] | n >= 1 && n <= length shapes -> do
              let sh = shapes !! (n-1)
              renderShapeWithAxes sh
            _ -> putStrLn "❌ Неверный номер."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef

    "5" -> do
      shapes <- readIORef shapesRef
      if null shapes
        then do
          putStrLn "❌ Нет фигур для удаления."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef
        else do
          mIdx <- selectIndex shapes
          case mIdx of
            Nothing -> putStrLn "❌ Удаление отменено."
            Just idx -> do
              let newShapes = take idx shapes ++ drop (idx + 1) shapes
              writeIORef shapesRef newShapes
              putStrLn "✅ Фигура удалена."
          putStrLn "\nНажмите Enter для возврата в меню..."
          _ <- getLine
          menuLoop shapesRef

    "6" -> do
      clearScreen
      putStrLn "Выход из программы. До свидания!"

    _ -> do
      putStrLn "⚠️ Неверный ввод. Выберите 1–6."
      putStrLn "\nНажмите Enter для продолжения..."
      _ <- getLine
      menuLoop shapesRef