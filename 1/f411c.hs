f411c :: Integer -> Double
f411c n = (m^2 -1)/(2*m^2 - m - 1) where m = fromIntegral n

limit' :: Double -> Integer -> Double
limit' e n = if abs(f411c n - (1/2)) < e
                then f411c n
                else limit' e (n+1)

limit :: Double -> Double
limit e = limit' e 1

{-
*Main> limit 0.001
0.500998003992016
-}
