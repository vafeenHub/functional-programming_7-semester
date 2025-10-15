-- | Функция быстрой сортировки
-- Принимает на вход список элементов, которые можно сравнивать (Ord)
-- Возвращает отсортированный список по возрастанию
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:rest) =
  let smallerSorted = quicksort [x | x <- rest, x <= pivot]
      biggerSorted  = quicksort [x | x <- rest, x > pivot]
  in smallerSorted ++ [pivot] ++ biggerSorted

main :: IO ()
main = do
  let unsortedList = [5, 3, 8, 1, 2, 7, 4, 6]
      sortedList = quicksort unsortedList
  putStrLn "Исходный список:"
  print unsortedList
  putStrLn "Отсортированный список:"
  print sortedList
