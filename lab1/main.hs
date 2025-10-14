-- 3) 21, 26, 52, 53, 59, 2.2 
-- Функция для вычисления средней урожайности и общего количества пшеницы
-- hectares - список полей в гектарах
-- yields - список урожайности (тонн с гектара)
-- Возвращает (средняя урожайность по всей территории, общее количество пшеницы)
calculateWheatStats :: [Double] -> [Double] -> (Double, Double)
calculateWheatStats hectares yields =
    let totalHectares = sum hectares
        totalYield = sum $ zipWith (*) hectares yields
        averageYield = if totalHectares == 0 then 0 else totalYield / totalHectares
    in (averageYield, totalYield)

main :: IO ()
main = do
    let hectares = [10.0, 20.0, 15.0]        -- Пример данных: hectares посеянных гектаров по районам
    let yields = [2.5, 3.0, 2.0]             -- Пример данных: урожайность тонн с гектара по районам

    let (avgYield, totalWheat) = calculateWheatStats hectares yields

    putStrLn $ "Средняя урожайность по всей территории: " ++ show avgYield ++ " тонн/га"
    putStrLn $ "Общее количество собранной пшеницы: " ++ show totalWheat ++ " тонн"
