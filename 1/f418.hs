f418 :: Integer -> Double
f418 n = (m^2 - 5*m + 6)/(m^2 - 8*m + 15) where m = fromIntegral n

limit' :: Double -> Integer -> Double
limit' e n = if abs(f418 n + 1) < e
                then f418 n
                else limit' e (n-1)

limit :: Double -> Double
limit e = limit' e 90