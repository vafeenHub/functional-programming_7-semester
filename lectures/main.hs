import Data.Char (isUpper)

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

main :: IO ()
main = do
    putStrLn $ delAllUpper "HELLO world HASKELL Program"
    putStrLn $ delAllUpper "ABC def GHI jkl"