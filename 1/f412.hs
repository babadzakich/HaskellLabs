f412 :: Integer -> Double
f412 n = ((1 + m)* (1 + 2*m)*(1+3*m)-1)/(m) where m = fromIntegral n

limit' :: Double -> Integer -> Double
limit' e n = if abs(f412 n - 6) < e
                then f412 n
                else limit' e (n+1)

limit :: Double -> Double
limit e = limit' e 90