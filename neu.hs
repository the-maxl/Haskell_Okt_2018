-- bitwise and
{-
ba 0 0 = 0
ba x y = if (x `mod` 2 + y `mod` 2) == 2
then 1 + 2 * ba (x `div` 2) (y `div` 2)
else 2 * ba (x `div` 2) (y `div` 2)
-}

{- even gibt es schon
uneven :: Integral a => a -> Bool
uneven x = if mod x 2 == 1
then True
else False
-}

--- odd    uneven x = not (even x)

div2 :: Integral a => a -> a
div2 x = x `div` 2

-- (&) :: Integer -> Integer
(&) 0 0 = 0
(&) x y = if odd x && odd y
then 1 + 2 *  div2 x & div2 y 
else 2 * div2 x & div2 y 
