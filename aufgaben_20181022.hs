-- Kapitel Starting Out
-- Eigene Übungsaufgaben 

-- Fakultät berechnen: (20181022)
facV1 x = if x == 0 then 1 else x * facV1 (x -1)

fac x -- (20181028)
  | x == 0 = 1
  | otherwise = x * fac (x-1)

-- Fibonacci-Folge: (20181022)
fibonacciV1 x = if x == 0
  then 0
  else (if x == 1
        then 1
        else fibonacciV1 (x-2) + fibonacciV1 (x-1))

fibonacci x       -- (20181028)
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fibonacci (x-2) + fibonacci (x-1)
  
fibonacciTest bis = [fibonacci x | x <- [0..bis]]

-- Elemente einer Liste summieren: (20181028)
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs


-- Liste mit Zweierpotenzen erzeugen:
zweierpotenzen bis = if bis == 0
  then [1]
  else (zweierpotenzen (bis-1) ++ [2^bis])

-- Liste mit allen Zweierpotenzen ab 2^0
gib2hoch = [2^x | x <- [0..]]   -- test: take 10 gib2hoch


