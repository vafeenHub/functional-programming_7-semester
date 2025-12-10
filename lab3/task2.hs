-- С помощью монады Writer и do нотации написать функцию, которая будет имитировать сдачу сессии студентом, то есть монада Writer должна уметь сохранять в себе список предметов впорядке их сдаче и сумму всех баллов за экзамены. Реализовать функции, которые будут из монады Writer доставать список сданных предметов в порядке их сдачи, и средний балл за все экзамены. Подсказка: в монаде Writer в качестве типа данных для логирования можно использовать кортеж, будет удобнее, если реализовать следующие функции. 
-- 3.1. session :: Writer ([String], ?) () - эта функция будет имитировать сдачу сессии.
-- 3.2. passExam :: String → Int → Writer ([String], ?) () - эта функция принимает в качестве параметров название предмета и оценку за него.
-- 3.3. getObejects :: Writer ([String], ?) () → [String] -- функция возвращает список сданных предметов.
-- 3.4. getAverageScore :: Writer ([String], ?) () → Int -- функция возвращает средний балл за сессию. 
-- На месте ? нужно использовать какой-то тип данных, подумать какой. Балльная система может быть от 2 до 5, или например от 0 до 100.

import Control.Monad.Writer
import Data.List (intercalate)

-- Определяем свой тип для лога с экземпляром Semigroup и Monoid
data SessionLog = SessionLog {
    subjects :: [String],
    totalScore :: Int,
    examCount :: Int
} deriving (Show)

instance Semigroup SessionLog where
    (SessionLog s1 t1 c1) <> (SessionLog s2 t2 c2) = 
        SessionLog (s1 ++ s2) (t1 + t2) (c1 + c2)

instance Monoid SessionLog where
    mempty = SessionLog [] 0 0

type SessionWriter = Writer SessionLog ()

-- 3.2 Функция для сдачи одного экзамена
passExam :: String -> Int -> SessionWriter
passExam subject score = do
    tell $ SessionLog [subject] score 1

-- 3.1 Функция, имитирующая сдачу сессии
session :: SessionWriter
session = do
    passExam "Математика" 85
    passExam "Физика" 92
    passExam "Программирование" 78
    passExam "Английский язык" 88

-- Дополнительный пример сессии
session2 :: SessionWriter
session2 = do
    passExam "История" 75
    passExam "Философия" 82
    passExam "Экономика" 90

-- 3.3 Функция для получения списка сданных предметов
getObjects :: SessionWriter -> [String]
getObjects sessionWriter = 
    let log = execWriter sessionWriter
    in subjects log

-- 3.4 Функция для получения среднего балла
getAverageScore :: SessionWriter -> Int
getAverageScore sessionWriter =
    let log = execWriter sessionWriter
    in if examCount log == 0 
       then 0 
       else totalScore log `div` examCount log

-- Вспомогательная функция для объединения нескольких сессий
combineSessions :: [SessionWriter] -> SessionWriter
combineSessions sessions = do
    sequence_ sessions

-- Функция для демонстрации работы
demoSession :: IO ()
demoSession = do
    putStrLn "=== Демонстрация работы функций ==="
    putStrLn ""
    
    putStrLn "1. Тестовая сессия:"
    let subjects1 = getObjects session
    let avg1 = getAverageScore session
    putStrLn $ "Сданные предметы: " ++ intercalate ", " subjects1
    putStrLn $ "Средний балл: " ++ show avg1
    putStrLn ""
    
    putStrLn "2. Вторая тестовая сессия:"
    let subjects2 = getObjects session2
    let avg2 = getAverageScore session2
    putStrLn $ "Сданные предметы: " ++ intercalate ", " subjects2
    putStrLn $ "Средний балл: " ++ show avg2
    putStrLn ""
    
    putStrLn "3. Объединенная сессия:"
    let combinedSession = do
            session
            session2
    let combinedSubjects = getObjects combinedSession
    let combinedAvg = getAverageScore combinedSession
    putStrLn $ "Все сданные предметы: " ++ intercalate ", " combinedSubjects
    putStrLn $ "Общий средний балл: " ++ show combinedAvg
    putStrLn ""
    
    putStrLn "4. Пустая сессия:"
    let emptySession = return ()
    let emptySubjects = getObjects emptySession
    let emptyAvg = getAverageScore emptySession
    putStrLn $ "Сданные предметы: " ++ show emptySubjects
    putStrLn $ "Средний балл: " ++ show emptyAvg

customSession :: [(String, Int)] -> SessionWriter
customSession exams = mapM_ (uncurry passExam) exams

main :: IO ()
main = do
    demoSession

    
    -- Сессия с разным количеством экзаменов
    putStrLn "\nСессия из одного экзамена"
    let singleExam = customSession [("Химия", 95)]
    putStrLn $ "Предметы: " ++ intercalate ", " (getObjects singleExam)
    putStrLn $ "Средний балл: " ++ show (getAverageScore singleExam)
    
    -- Сессия с низкими баллами
    putStrLn "\nСессия с низкими баллами"
    let lowScores = customSession [("Литература", 60), ("Биология", 65)]
    putStrLn $ "Предметы: " ++ intercalate ", " (getObjects lowScores)
    putStrLn $ "Средний балл: " ++ show (getAverageScore lowScores)
    
    -- Проверка порядка сдачи предметов
    putStrLn "\nПроверка порядка сдачи"
    let orderedSession = customSession $
            [("Предмет1", 100), ("Предмет2", 90), ("Предмет3", 80)]
    putStrLn $ "Порядок сдачи: " ++ intercalate " → " (getObjects orderedSession)
    putStrLn $ "Средний балл: " ++ show (getAverageScore orderedSession)
    
    -- Использование combineSessions
    putStrLn "\nИспользование combineSessions"
    let sessionsList = [session, session2]
    let combined = combineSessions sessionsList
    putStrLn $ "Всего предметов: " ++ show (length $ getObjects combined)
    putStrLn $ "Общий средний балл: " ++ show (getAverageScore combined)
    
    -- Сессия с баллами от 0 до 100
    putStrLn "\nСессия с разными типами оценок"
    let hundredScaleSession = customSession [
            ("Алгебра", 67),
            ("Геометрия", 89),
            ("Тригонометрия", 73)
            ]
    putStrLn $ "Предметы: " ++ intercalate ", " (getObjects hundredScaleSession)
    putStrLn $ "Средний балл: " ++ show (getAverageScore hundredScaleSession)