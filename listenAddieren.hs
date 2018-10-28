-- in ghci:
-- :l sum.hs

-- gleich lange Listen addieren
sum1 x y = if length x == 1
    then [head x + head y]
    else [head x + head y] ++ sum1 (tail x) (tail y)

-- Listen addieren
sum2 x y = if length x == 0
     then y
     else if length y == 0
     then x
     else [head x + head y] ++ sum2 (tail x) (tail y)

testSum22 = if sum2 [1,2] [3,4,5] == [4,6,5]
then "ok"
else "falsch"

testSum2 = if elem False [sum2 [] [] == [], sum2 [1,2] [3,4,5] == [4,6,5], sum2 [0] [] == [0], sum2 [2] [0] == [2]] == False
then "test bestanden"
else "Fehler"

sort1 x = if length x == 0
then []
else [minimum x]  ++  sort1 (tail x)

sort = 123