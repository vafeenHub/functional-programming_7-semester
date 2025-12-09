-- [v] a -> b [r] - Functor
-- [v] [a -> b] [r] - Applicative
-- [v] [a] -> b [r] - Applicative
-- [v] a -> [b] [r] - Monad

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (guard)

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   x >> y = x >>= \_ -> y
--   return :: a -> m a


--instance Monad Maybe where
  --return x = Just x
  --Nothing >>= f = Nothing
  --Just x >>= f = f x


f1 = Just 5 >> Nothing >> Just [1..4] >> Just 3


sayHello :: String -> String
sayHello name = "Hello, " ++ name

printInConsole = putStrLn "Name: " >> getLine >>= 
    (\ name -> return . sayHello $ name) >>= putStrLn

printInConsoleDo :: IO ()
printInConsoleDo = do
    putStrLn "Name: "
    name <- getLine
    let st = sayHello name
    putStrLn st  



int1 = Nothing
int2 = Just 6

multMaybe :: Maybe Int
multMaybe = do
    a <- int1
    b <- int2
    return $ a * b 

--instance Monad [] where
  --return x = [x]
  --xs >>= f = concat . map f $ xs

l1 :: Int -> [(Char, Int)]
l1 n = do
    a <- if n < 10 then [1..3] else [4 .. 6]
    b <- ['a' .. 'c']
    guard $ even a
    return (b, a)


-- newType Writer w a = Writer {runWriter :: (a, w)}

--instance Monad (Writer w) where
  --return x = Writer (x, mempty)
  --(Writer (x, v)) >>= f = Writer (y, v `mappend` v')
    --where f x = Writer (y, v') 


logNumber :: Int -> Writer [String] Int
logNumber n = writer (n, ["Number " ++ show n])

logNumberStr :: String -> Writer [String] String
logNumberStr str = writer (str, ["Number " ++ str])

multWithLog :: Int -> Int -> Writer [String] Int
multWithLog x y = do
    a <- logNumber x
    b <- logNumberStr (show y)
    tell ["Mult x and y"]
    return $ a * (read b :: Int)        


-- Reader ~ (->) r

--instance Monad ((->) r) where
  --return x = \ _ -> x
  --h >>= f = \ w -> f (h w) w 

r1 :: Int -> Int
r1 = do
    a <- (*2)
    b <- (+10)
    c <- show
    return (a + b - length c)      


-- newType State s a = State {runState :: s -> (a, s)}
--instance Monad (State s) where
  --return x State $ \ s -> (x, s)
  --(State h) >>= f = State $ let (a, newState) = h s
                            --(State g) = f a
                            --in g newState

type Stack = [Int]

pop :: State Stack Int
pop = state $ \ (x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \ xs -> ((), a:xs)

--s1 :: State Stack ()
s1 = do
    v <- pop
    s <- get
    put []
    push (5 + (head s))
    --push (v + 1)
    pop        