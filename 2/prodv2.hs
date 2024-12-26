solver :: (Ord t1, Fractional t2, Num t1) => t2 -> t2 -> (t2 -> t1) -> t1 -> t2
solver a b f eps
    |check a b f < eps = dihot a b
    |f (dihot a b) < 0 = solver (dihot a b) b f eps
    | otherwise = solver a (dihot a b) f eps
{-f x = x**2 - 2 **x, solver (-4) 4 f 1 = 2.0, solver (-4) 0 f 4 = -2 -}
{-f x = (2**x) - (x**2), solver (-4) 0 f 1 = -1 -}
    
dihot :: Fractional a => a -> a -> a
dihot a b =((a+b)/2)  
-- шаг дихотомии
check :: (Fractional t, Num a) => t -> t -> (t -> a) -> a
check a b f = abs(f (dihot a b))
-- check возводит дихотомию в абсолют для проверки точности


arct :: Double -> Double -> Double
arct x 0 = 0
arct x n = ((-1)**(n+1)) *((x**(2*n-1))/(2*n-1)) + arct x (n-1)

pifind :: Double -> Double
pifind n = 4 * arct 1 n

fibtup n
    | n <= 1 = 1
    | n > 1 = fibtup (n-1) + fibtup(n-2)
-- фибоначи в лоб 
fr a b 0 = b
fr a b n = fr b (a+b) (n-1)
fibumn n = fr 1 1 n
-- ищет 500000 число за 30 сек