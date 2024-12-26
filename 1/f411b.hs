f411b :: Integer -> Double
f411b n = (m^2 -1)/(2*m^2 - m - 1) where m = fromIntegral n

limit' :: Double -> Integer -> Double
limit' e n = if abs(f411b n - (2/3)) < e
                then f411b n
                else limit' e (n-1)

limit :: Double -> Double
limit e = limit' e 90