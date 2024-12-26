f411a :: Integer -> Double
f411a n = (m^2 -1)/(2*m^2 - m - 1) where m = fromIntegral n

limit'1 :: Double -> Integer -> Double
limit'1 e n = if abs(f411a (1`div`m) - 1) < e
                then f411a (1 `div` m)
                else limit'1 e (n+1)
                where m = fromIntegral n

limit1 :: Double -> Double
limit1 e = limit'1 e 2

f412 :: Integer -> Double
f412 n = ((1 + m)* (1 + 2*m)*(1+3*m)-1)/(m) where m = fromIntegral n

limit'2 :: Double -> Integer -> Double
limit'2 e n = if abs(f412 (1`div`m) - 1) < e
                then f412 (1 `div` m)
                else limit'2 e (n+1)
                where m = fromIntegral n

limit2 :: Double -> Double
limit2 e = limit'2 e 6