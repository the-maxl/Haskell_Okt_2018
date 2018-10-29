-- Alt-m haskell-mode

abs' x = if x >= 0
  then x
  else (-1) * x

length' [] = 0
length' (_:xs) = 1+ length' xs


sum' [] = 0
sum' (x:xs) = x + sum' xs


odd' :: Integral a => a -> Bool
odd' x
  | (x `mod` 2) == 1 = True
  | otherwise = False

--- odd    uneven x = not (even x)

-- Bitweises and - Version 1 (20181026)
-- div2 :: Integral a => a -> a
-- div2 x = x `div` 2
-- (&) 0 0 = 0
-- (&) x y = if odd x && odd y
--  then 1 + 2 *  div2 x & div2 y 
--  else 2 * div2 x & div2 y 

(&) 0 0 = 0          --(20181028)
(&) x y 
  | odd x && odd y = 1 + rest
  | otherwise = rest
  where  rest = 2 * (div2 x & div2 y)
         div2 x = x `div` 2

(!=) a b = if a == b then True else False

-- Primzahlen
istPrim 2 = True
istPrim x = not (0 `elem` zipWith mod [x,x..] [2..(x `div` 2)])


istPrim' x  -- ausführliche Verdion 20181029
  | x == 2 = True
  | otherwise =  not teilerEx
  where teiler = [2..(x `div` 2)] -- liste mit allen möglichen Teilern 
        ergebnisse = zipWith mod (repeat x) teiler
        teilerEx = 0 `elem` ergebnisse
        
  
-- Primzahlen bis <to> ausgeben
primBis to = [x | x <- [1..to], istPrim x]


-- Wurzelberechnung
    -- x^(1/2) = s + e     -- s: start value, e: error
    -- x = s^2 + 2se + e^2  -- e^2 is small and will be neglected
    -- e = (x - s^2)/(2s)  -- better start value: s + e

sqrt' :: (Ord t, Fractional t) => t -> t
sqrt' x = iterate x 1 1e-15
  where iterate x est err = if (errorSmall x est err)
          then est                               -- estimated value is good enough
          else iterate x (est + getError x est) err     -- use better estimation
        getError x est = (x - est^2)/(2 * est)
        errorSmall x est err = abs (getError x est) < err


sqrtFromTo from to = [(x,y) | x <- [from..to], let y = sqrt' x]


-- "radikaler" Ansatz
-- Wurzel mit list comprehension (falls Wurzel ganzzahlig)
-- alt sqrtInt xs = snd (head [(x,y) | x <- [xs], y <- [1..], x == y*y ])

sqrtInt x = head [w | w <- [0..], w^2 == x]

-- Liste kleiner float-Zahlen 
floatList step = zipWith (/) [0..] (repeat step)
--take 10 step 1e5  --> [0.0,1.0e-5,2.0e-5,3.0e-5,4.0e-5,5.0e-5,6.0e-5, ...

-- Wurzel aus Float, Schrittweite muss kleiner als tolerierter Fehler sein
sqrtFloat x = head [w | w <- floatList 1e5, abs(w^2 - x ) < 1e-4]
-- so lassen sich auch andere Probleme annähern, z.B. 6 = x^2 + x

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

