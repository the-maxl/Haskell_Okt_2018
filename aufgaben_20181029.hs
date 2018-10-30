-- pattern recognition
jutta = ("Jutta", "Maier", 217859)
peter = ("Peter", "Penner", 0815)
paula = ("Paula", "Kurz", 123321123)
heinz = ("Heinz", "Kunz", 111222)

-- Nachname aus tupel bekommen
-- (_,nachname,_) = jutta    --> nachname = "Maier"
-- Anfangsbuchstaben aus String bekommen
-- (i:_) = name          --> i = 'M'

-- Anfangsbuchstaben des Nachnamens aus tupel:
-- (_,(i:_),_) = peter       --> i = 'P'


l = [jutta, peter, paula, heinz]

gibEintrag o = vorname ++ " " ++ nachname ++ ". "++ show nummer 
  where (vorname,nachname ,nummer) = o

gibEintraege l
  | length l == 0 = ""
  | otherwise = (gibEintrag (head l)) ++ " - " ++ gibEintraege (tail l)

gibInitialien o = vorname ++ [n]
  where (vorname, nachname,_) = o
        (n:_) = nachname


--t o c = doo
  --where 
namensanfang name 'K' = "mit K"
namensanfang name _ = "nicht mit K"

cn name = case name of ('A':_) -> "Beginnt mit A"
                       ('B':_) -> "Beginnt mit B"
                       (x) -> "Beginnt nicht mit A oder B"


c x = case x of (1,_) -> "beginnt mit eins"
                (2,_) -> "beginnt mit zwei"
                (x,_) -> "beginnt mit " ++ show x
                --(_,_,_) -> "ein Tripel"
               -- ([]) -> "leere Liste"

c2 x = case x of [] -> "empty List"
                 [a] -> "ein Element"
                 [a,b] -> "zwei Elemente"
                 x -> "mehr als zwei Elemente"
                
              --  ("s") -> "string"
               -- ([x]) -> "ein eintrag"
              --  [x:xs] -> "list"
             --   ((x,y))-> "tupel"
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  


-- Kapitel Rekursion

replicate' 0 what = []
replicate' x what = what:(replicate' (x-1) what)

take' n list
  | list == [] = []
  | n <= 0 = []
  | otherwise = (head list) : (take' (n-1) (tail list))


reverse' list = case list of [] -> []
                             [x] -> [x]
                             (x:xs) -> (reverse' xs) ++ [x]


repeat' a = a:[] ++ (repeat' a)

zip' l1 [] = []
zip' [] l1 = []
zip' l1 l2 = (head l1, head l2) : zip' (tail l1) (tail l2)

elem' item [] = False
-- elem' item [x] = x == item  -- Überflüssig
elem' item (x:xs) = (x == item) || elem' item xs


-- Kapitel Higher order functions


mal5 x = x*5
e  = mal5 6

t f x = f x

-- parameter tauschen
changeParm f x y = f y x  -- ch (++) " zwei" "eins" --> "eins zwei" -  ch (/) 2 7 --> 3.5

quad f x = (f x)^2        -- quad sqrt 4 --> 4


unzip' f x = case x of [] ->  []
                       [x] -> [f x]
                       (x:xs) -> f x : unzip' f xs

-- unzip fst [(1,"a"), (2,"b"), (3,"c")]  --> [1,2,3]
-- unzip snd [(1,"a"), (2,"b"), (3,"c")]  --> ["a","b","c"]

map' f l = case l of [] -> []
                     [x] -> [f x]
                     (x:xs) -> f x : map' f xs

-- map' (+ 4) [0,1,2,9]  -> [4,5,6,13]

{-
*Main> ziel x y = x + y
*Main> ziel 3 4
7
*Main> t1 = ziel 3
*Main> t1 4

*Main> xp x = (+) x
*Main> xp 1 2
3
-}

-- Variable Anzahl von Argumenten in Liste
--toList = []
toList x = [x]


--20181030
-- let (x:xs) = [1,5,2,8,2,9]  --- > x = 1, xs = [5,2,8,2,9]

--Bubblesort (aus Gedächtnis)
bsort l = case l of
  [] -> []
  [x] -> [x]
  (x:xs) -> bsort (kleinerGleich x xs) ++ [x] ++ bsort (groesserAls x xs)
  where kleinerGleich x xs = [e | e <- xs, e <= x]
        groesserAls x xs = [e | e <- xs, e > x]

-- bsort ["so", "geht", "das"]  --> ["das","geht","so"]


-- bestimmte Elemente aus Liste extrahieren:
filter' bed x = [e | e <- x, bed e]  -- filter' (>=2) [1,2,3,-1, 0]  --> [2,3]

-- mit Lambda
-- filter' (\x -> x>2 && x<8) [1..100]   --> [3,4,5,6,7]

-- bsort' [] = []
-- bsort' [x] = [x]
-- bsort' (x:xs) = bsort' (select (<= x) xs) ++ [x] ++ bsort' (select (> x) xs)
--   where select f x = [e | e <- x, f e]
bsort' [] = []
bsort' [x] = [x]
bsort' (x:xs) = bsort' (filter' (<= x) xs) ++ [x] ++ bsort' (filter' (> x) xs)

-- bsort' [1,2,3,-1]  --> [-1,0,1,2,3]



-- doppelte Elemente aus Liste entfernen
remDouble [] = []
remDouble (x:xs) = [e | e <- [x], not(x `elem` xs)] ++ remDouble xs

-- remDouble [1,5,2,8,2,11,9] --> [1,5,8,2,11,9]

-- verknüpfen von Bedingungen
{-
let bed1 = (>2)
let bed2 = (<1)
let bed = bed1 || bed2
-}

--bed1 :: (Num a)=> Bool --(Num a) => a -> Bool
bed1 = (>5)
bed2 = (<2)
bed x = (bed1 x) || (bed2 x)
-- (\x -> (bed1 x) || (bed2 x)) 8
-- (\x -> x>5 || x<2) 3

-- select (\x -> x^2 `elem` [0..10]) [0..32]  --> [0,1,2,3]


satz = ["Das", "ist", "ein", "kurzer", "Satz", "mit", "ein", "paar", "längeren", "Worten"]

-- Worte mit Länge <= 4
-- filter' (\x -> (length x) <= 4)  satz  --> ["Das","ist","ein","Satz","mit","ein","paar"]

-- Worte mit Länge 4
-- filter' (\x -> (length x) == 4)  satz  --> ["Satz","paar"]

-- Worte die mit Großbuchstaben beginnen:
-- filter' (\(x:xs) -> x `elem` ['A'..'Z'])  satz  --> ["Das","Satz","Worten"]

-- Worte mit 'a' als zweitem Buchstaben
-- filter' (\(x:y:xs) -> y == 'a')  satz  --> ["Das","Satz","paar"]

  -- falls einzelne Buchstaben:
  -- *** Exception: <interactive>:650:9-29: Non-exhaustive patterns in lambda
  -- WIE LASSEN SICH LAMBDAS VERKNÜPFEN
-- And like normal functions, you can pattern match in lambdas. The only difference is that you can't define several patterns for one parameter, like making a [] and a (x:xs) pattern for the same parameter and then having values fall through.
-- zipWith (+) [1,2,3] [4,3,2,1,0]   --> [5,5,5]

l1 = [1,2,3]
l2 =[4,3,2,1,0]

-- Vorübung: map' (\(x,y) -> x + y) z  -->  [5,5,5]
zipWith' f l1 l2 = map' (\(x,y) -> f x y) (zip' l1 l2)
-- zipWith (-) l1 l2   --> [-3,-1,1]


takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x:takeWhile' p xs else[]  -- p: predicate 

-- takeWhile' (\x -> length x <=3) ["Das", "ist", "lediglich", "ein", "Test"]  -->["Das","ist"]

{- V1
collatz x
  | x == 1 = []
  | x `mod` 2 == 0  = (div2 x) : (collatz (div2 x))
  | otherwise = (m x) : (collatz (m x))
  where div2 x = x `div` 2
        m x = 3*x+1
-}

collatz x
  | x == 1 = [1]
  | x `mod` 2 == 0  = x : (collatz (x `div` 2))
  | otherwise =  x : (collatz (3*x +1))

--length (filter (\x -> length x > 15) (map collatz [1..100]))  --> 66



operationen x = [(x+), (x-), (x*), (x/)]
-- (operationen 2 !! 0) 8   --> 10.0
-- (operationen 2 !! 1) 8   --> -6.0

re x y =
  let ops x = [(x+), (x-), (x*), (x/)]
  in ((ops x) !! 1) y


-- fold
foldl' _ a [] = a
foldl' f a [x] = f a x
foldl' f a (x:xs) = foldl' f (f a x) xs
