module Part3 where

import Data.List (find, group, nub, sort)
------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = getPrimeDivisors n == [n]

getPrimeDivisors :: Integer -> [Integer]
getPrimeDivisors = currentDivisors 2
  where
    currentDivisors :: Integer -> Integer -> [Integer]
    currentDivisors _ 1 = []
    currentDivisors d num
        | d * d > num = [num]
        | num `mod` d == 0 = d : currentDivisors d (num `div` d)
        | otherwise = currentDivisors (d + 1) num        

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 number = map (\divisors -> (head divisors, length divisors)) groupEqualDivisors
    where
        groupEqualDivisors :: [[Integer]]
        groupEqualDivisors = group (getPrimeDivisors number)

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
getDivisors :: Integer -> [Integer]
getDivisors 1 = [1]
getDivisors n = (1 :) $ nub $ concat [[x, n `div` x] | x <- [2 .. limit], n `rem` x == 0]
  where
    limit = (floor . sqrt . fromIntegral) n

prob20 :: Integer -> Bool
prob20 1 = False
prob20 n = n == sum (getDivisors n)
-- prob20 1 = False
------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 1 = [1]
prob21 n = (sort . getDivisors) n ++ [n]
-- prob21 1 = [1]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 input = product $ (map lettersCount) (words input)
    where
        lettersCount :: String -> Integer
        lettersCount word = toInteger $ length (filter (=='i') word)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 inputString = return inputString >>= parseInput >>= getSlice
    where
        parseInput :: String -> Maybe ParseResult
        parseInput input = do
            let left = read $ takeWhile (/= '-') input
            let right = read $ takeWhile (/= ':') $ tail $ dropWhile (/= '-') input
            let string = tail $ dropWhile (/= ' ') input
            return ParseResult { leftBound = left, rightBound = right, stringToSlice = string }

        getSlice :: ParseResult -> Maybe String
        getSlice (ParseResult left right string)
            | left > length string || right > length string = Nothing
            | right >= left = Just $ leftToRightSlice left right
            | otherwise = Just $ reverse $ leftToRightSlice right left
            where
                leftToRightSlice :: Int -> Int -> String
                leftToRightSlice l r = take r $ drop (l - 1) string

data ParseResult = ParseResult
    {
        leftBound :: Int,
        rightBound :: Int,
        stringToSlice :: String
    }

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = checkNum(sqrt (1 + 8 * fromInteger num)) == 0
  where
    checkNum x = x - fromIntegral (floor x)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = isPalindrome . show
  where
    isPalindrome xs = xs == reverse xs

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 left right = (sum . getDivisors) left == right && (sum . getDivisors) right == left

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 s (x:xs) = if s - x `elem` xs 
                    then Just (x, s - x)
                    else prob27 s xs

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 requiredSum inputList = do
    list <- find
            (\list -> sum list == requiredSum)
            $ subsets 4 inputList
    return (list !! 3, list !! 2, list !! 1, list !! 0)

    where
        subsets :: Int -> [a] -> [[a]]
        subsets subLength listToHandle =
            if subLength > length listToHandle
            then []
            else subsequencesBySize listToHandle !! (length listToHandle - subLength)

        subsequencesBySize [] = [[[]]]
        subsequencesBySize (curHead : curTail) =
            let next = subsequencesBySize curTail
            in zipWith (++) ([] : next) (map (map (curHead :)) next ++ [[]])
------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = maximum [x * y|
    x <- [minNumber..maxNumber],
    y <-[minNumber..maxNumber],
    (prob25 . toInteger) (x*y)]
      where
        minNumber = 10 ^ (k - 1)
        maxNumber = 10 ^ k - 1

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 reqCount = head $
    filter (\triangular -> (length . getAllDivisors) triangular >= reqCount)
    triangularNumbers
      where
          getAllDivisors :: Integer -> [Integer]
          getAllDivisors n = (if n == 0 then [] else [n]) ++ getDivisors n

triangularNumbers :: [Integer]
triangularNumbers = triangularWithCurrent 0 1
     where
           triangularWithCurrent :: Integer -> Integer -> [Integer]
           triangularWithCurrent current next = current : triangularWithCurrent (current + next) (succ next)       

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 n = sum [
                x + y |x <- [1 .. n],
                y <- [x+1 .. n],
                prob26 (toInteger x) (toInteger y)
            ]

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins moneySum
    | moneySum < minimum coins = []
    | otherwise = [
            coin : nextCoins |
            coin <- reverse coins,
            nextCoins <- [] : prob32 (filter (<= coin) coins) (moneySum - coin),
            sum (coin : nextCoins) == moneySum
        ]
