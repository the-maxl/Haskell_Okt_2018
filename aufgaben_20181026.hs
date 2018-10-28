

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
