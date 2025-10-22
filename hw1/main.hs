-- | Определяем порядок дней недели (Monday - 1, Sunday - 7)
dayOrder :: String -> Int
dayOrder day = case day of
  "Monday"    -> 1
  "Tuesday"   -> 2
  "Wednesday" -> 3
  "Thursday"  -> 4
  "Friday"    -> 5
  "Saturday"  -> 6
  "Sunday"    -> 7
  _           -> 100 -- Для неизвестных значений

-- | Быстрая сортировка для дней недели с использованием dayOrder
dayQuickSortDays :: [String] -> [String]
dayQuickSortDays [] = []
dayQuickSortDays (pivot:rest) =
  let smallerSorted = dayQuickSortDays [x | x <- rest, dayOrder x <= dayOrder pivot]
      biggerSorted  = dayQuickSortDays [x | x <- rest, dayOrder x > dayOrder pivot]
  in smallerSorted ++ pivot : biggerSorted

-- | Ваша универсальная функция быстрой сортировки для типов с Ord
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot : rest) =
  let smallerSorted = quicksort [x | x <- rest, x <= pivot]
      biggerSorted = quicksort [x | x <- rest, x > pivot]
  in smallerSorted ++ [pivot] ++ biggerSorted

main :: IO ()
main = do
  let unsortedNumbers = [5, 3, 8, 1, 2, 7, 4, 6]
      sortedNumbers = quicksort unsortedNumbers

      unsortedStrings = ["apple", "banana", "orange", "pear", "kiwi"]
      sortedStrings = quicksort unsortedStrings

      unsortedDays = [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
      sortedDays = dayQuickSortDays unsortedDays

  putStrLn "Исходный список чисел:"
  print unsortedNumbers
  putStrLn "Отсортированный список чисел:"
  print sortedNumbers

  putStrLn "\nИсходный список строк:"
  print unsortedStrings
  putStrLn "Отсортированный список строк:"
  print sortedStrings

  putStrLn "\nИсходный список дней недели:"
  print unsortedDays
  putStrLn "Отсортированный список дней недели:"
  print sortedDays
