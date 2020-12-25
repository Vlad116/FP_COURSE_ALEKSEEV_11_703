module StepicIntro where

import Data.Char

-- Введение

sayHello :: IO ()
sayHello = putStrLn "Hello, world! From Test!"

sumSquares x y = x ^ 2 + y ^ 2

rock'and'roll :: Integer
rock'and'roll = 42

-- определение оператора своего
infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2

infixl 6 |-|

(|-|) :: (Ord a, Num a) => a -> a -> a
(|-|) x y = if x - y > 0 
    then x - y
    else - (x - y)

f $ x = f x

test = isDigit '1'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y 
    then digitToInt x * 10 + digitToInt y
    else 100
    
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2)

factorial n = if n == 0 
    then 1 
    else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = n * factorial'(n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 
    then error "arg must be >= 0"
    else n * factorial'(n - 1)

doubleFact :: Integer -> Integer
doubleFact n = if n > 0
    then n * doubleFact(n - 2) 
    else 1

factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0"
               | n > 0 = n * factorial'''(n - 1)

factorial4 :: (Num p, Ord p) => p -> p
factorial4 n | n == 0 = 1
             | n > 0 = n * factorial4(n - 1)
             | otherwise = error "arg must be >= 0"

fibonacci :: Integer -> Integer
fibonacci n | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1) 
            | otherwise = n

-- helper acc 0 = acc
-- helper acc n = helper (acc * n) (n - 1)

-- factorial5 :: (Num p, Ord p) => p -> p
-- factorial5 n | n >= 0    = helper 1 n
            --  | otherwise = error "arg must be >= 0"

fibonacci2 :: Integer -> Integer
fibonacci2 n = helper 0 1 n
  where helper curr prev n
          | n == 0   = curr
          | n > 0    = helper (curr+prev) curr (n-1)
          | n < 0    = helper prev (curr-prev) (n+1)

roots :: Double 
      -> Double
      -> Double
      -> (Double, Double)
roots a b c = 
    (
        (-b - sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
    ,
        (-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * a)
    )

-- let in
roots' a b c =
    let sq = sqrt (b ^ 2 - 4 * a * c)
        d = 2 * a
    in ((-b - sq) / d, (-b + sq) / d)

roots'' a b c =
    let 
        aTwice = 2 * a
        d = sqrt (b ^ 2 - 4 * a * c)
        x1 = (-b - d) / aTwice
        x2 = (-b + d) / aTwice
    in (x1, x2)

factorial6 :: (Num p, Ord p) => p -> p
factorial6 n 
    | n >= 0 = let
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
      in helper 1 n    
    | otherwise = error "arg must be >= 0"

rootsDiff a b c = let
    (x1, x2) = roots a b c
    in x2 - x1

{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
a[0] = 1
a[1] = 2
a[2] = 3
a[k+3] = a[k+2] + a[k+1] − 2a[k].
-}
seqA :: Integer -> Integer
seqA n = helper 1 2 3 n where
    helper a1 a2 a3 0 = a1
    helper a1 a2 a3 1 = a2
    helper a1 a2 a3 2 = a3
    helper a1 a2 a3 n = helper a2 a3 (a3 + a2 - 2 * a1) (n - 1)  

roots''' a b c = (x1, x2) where
        x1 = (-b - d) / aTwice
        x2 = (-b + d) / aTwice
        d = sqrt (b ^ 2 - 4 * a * c)
        aTwice = 2 * a

-- let x = 2 in x^2 - полноценное выражение
-- В то время как x^2 where x = 2 - не явл. выражением

factorial7 :: Integer -> Integer
factorial7 n | n >= 0   = helper 1 n
             | otherwise = error "arg must be >= 0"
    where
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)

-- sum'n'count :: Integer -> (Integer, Integer)
-- sum'n'count x | x == 0  = (0,0)
--               |

{-
    Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
    sum'n'count :: Integer -> (Integer, Integer)
    sum'n'count x = undefined
    GHCi> sum'n'count (-39)
    (12,2)
-}

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x  | x >= 0  = helper x 0 0 
               | x < 0   = helper (-x) 0 0
    where
        helper 0 0 0 = (0, 1)
        helper 0 sum count = (sum, count)
        helper x sum count = helper (x `div` 10) (sum + x `mod` 10) (count + 1)

-- Реализуйте функцию, находящую значение определённого интеграла от заданной функции ff на заданном интервале [a,b][a,b] методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = aHalf * (y a 0 0) where
    h = (b - a) / 1000
    aHalf = h / 2
    y x s i | a == 0 && b == 0 = 0 
            | i == 0           = y (a + h) (s + f a) (i + 1)
            | i == 1000        = s + (f b)
            | otherwise        = y (x + h) (s + 2 * f x) (i + 1)