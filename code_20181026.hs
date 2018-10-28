-- Alt-m haskell-mode

-- String einrücken
printIdent i text = if i > 0
then
   " " ++ printIdent (i-1) text
else
   text


-- Primzahlen
istPrim 2 = True
istPrim x = not (0 `elem` zipWith mod [x,x..] [2..x `div` 2])

-- Primzahlen bis <to> ausgeben
primBis to = [x | x <- [1..to], istPrim x]

-- gibt es schon, umbenennen:    abs x = if x >= 0 then x else (-1) * x

{-- Wurzelberechnung
    x^(1/2) = s + e     -- s: start value, e: error
    x = s^2 + 2se + e^2  -- e^2 is small and will be neglected
    e = (x - s^2)/(2s)  -- better start value: s + e
--}
-- get error
sqrtError x s = (x -s^2) / (2 * s)

-- iteration step
sqrtIterate x est error = if abs (sqrtError x est) < error
then est -- estimated value is good enough
else sqrtIterate x (est + sqrtError x est) error  -- use better estimation


sqrtMaxl x = sqrtIterate x 1 1e-5
  
sqrtFromTo from to = [(x,y) | x <- [from..to], let y = sqrtMaxl x]

-- Wurzel mit list comprehension (falls Wurzel ganzzahlig)
cs xs = snd (head [(x,y) | x <- [xs], y <- [1..], x == y*y ])

{--
ml x = if x == 0
then [0]
else (ml x-1) ++ [x]
--}

-- Liste mit Ganzzahlen von 0 bis x erzeugen
ml 0 = 0:[]
ml x = ml (x-1) ++ [x]

la liste = if length liste == 0
then 0
else head liste * 1e-2



err = 1e-5
dlist =  (zipWith (*) [1..] (repeat err))
csd xs = snd (head [(x,y) | x <- [xs], y <- dlist, x < y*y +0.01, x > y*y -0.01 ])
--csd xs = snd (head [(x,y) | x <- [xs], y <- dlist, x < y*y +err, x > y*y -err])


{-- ausführlicher
-- Primzahlen
istPrim 2 = True
istPrim x = if even x
then
   False
else
 not (0 `elem` zipWith mod [x,x..] [2..x `div` 2])
 --}
removeNotUppercase str = [b | b <- str, not (b `elem` ['A'..'Z'])]
-- removeNotUppercase "Ddas KKkomMmMt RrAaus."


-- Kapitel Types and Typeclasses
-- Typ bestimmen: > :t 'a'  -> 'a' :: Char
-- :: "has type of"


removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  


-- a any type

--Typeclasses 101

(!=) a b = if a == b then True else False
-- *Main> 1 != 2

-- Note: the equality operator, == is a function. So are +, *, -, / and pretty much all operators. If a function is comprised only of special characters, it's considered an infix function by default. If we want to examine its type, pass it to another function or call it as a prefix function, we have to surround it in parentheses.

-- Kapitel Syntax in Functions
-- The _ means the same thing as it does in list comprehensions. It means that we really don't care what that part is, so we just write a _.

--
--We already implemented our own length function using list comprehension. Now we'll do it by using pattern matching and a little recursion:

length' [] = 0
length' (_:xs) = 1+ length' xs


sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Bestimmung des kleinsten Wertes einer Liste: minimum
-- MK 20181027
min3 :: Ord a => a -> a -> a -> a  -- gib kleinsten von drei Werten
min3 a b c = min a (min b c)

miniHelp :: Ord a => [a] -> a -> a  -- Hilfsfunktion um bisher kleinsten Wert zu halten
miniHelp [] a = a
miniHelp [x] a = min x a
miniHelp (x:xs) a = min3 x (miniHelp xs a) a 

minimum' :: Ord a => [a] -> a
minimum' [] = error "Leere Liste"
minimum' [a] = a
minimum' x = miniHelp x (head x)

-- -- Liste bis zu erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
-- listeBisHelp ausgabe [] wert = error "Wert nicht gefunden"
-- listeBisHelp ausgabe liste wert = if (head liste) == wert
--   then ausgabe
--   else listeBisHelp (ausgabe ++ [head liste]) (tail liste) wert

-- listeBis liste wert = listeBisHelp [] liste wert


-- -- Liste ab dem erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
-- listeAb [x] wert = if x == wert
--   then []
--   else error "Wert nicht in Liste"
  
-- listeAb liste wert = if head liste == wert
--   then tail liste
--   else listeAb (tail liste) wert

-- removeFromList :: Eq t => [t] -> t -> [t]
-- removeFromList liste wert = (listeBis liste wert) ++ (listeAb liste wert)

-- -- Liste sortieren
-- sortHelp :: Ord a => [a] -> [a] -> [a]
-- sortHelp sorted [] = sorted
-- sortHelp sorted unsorted = sortHelp ([maximum unsorted] ++ sorted) (removeFromList unsorted (maximum unsorted))

-- sort :: Ord a => [a] -> [a]
-- sort liste = sortHelp [] liste


-- 20181028
--where
t = a ++ a ++ b
 where a = " horst "
       b = " waldi "


-- Liste bis zu erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
listeBis liste wert = listeBisHelp [] liste wert
  where listeBisHelp ausgabe [] wert = error "Wert nicht gefunden"
        listeBisHelp ausgabe liste wert = if (head liste) == wert
          then ausgabe
          else listeBisHelp (ausgabe ++ [head liste]) (tail liste) wert


-- Liste ab dem erstem Auftreten von wert zurückgeben (wert wird nicht zurückgegeben)
listeAb [x] wert = if x == wert
  then []
  else error "Wert nicht in Liste"
listeAb liste wert = if head liste == wert
  then tail liste
  else listeAb (tail liste) wert

removeFromList :: Eq t => [t] -> t -> [t]
removeFromList liste wert = (listeBis liste wert) ++ (listeAb liste wert)
 
-- Liste sortieren
sort :: Ord a => [a] -> [a]
sort liste = sortHelp [] liste
  where sortHelp :: Ord a => [a] -> [a] -> [a]
        sortHelp sorted [] = sorted
        sortHelp sorted unsorted = sortHelp (addMax (maximum unsorted) sorted) (remMax unsorted)
        addMax wert liste = [wert] ++ liste
        remMax  liste = removeFromList liste (maximum liste)
        

