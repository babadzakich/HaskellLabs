
-- 1 zadazha

sdvig :: [a] -> [[a]]
sdvig xs = sdvig' [xs] xs (length xs)
{- list = [1,2,3] sdvig list = [[1,2,3],[2,3,1],[3,1,2]]-}
sdvig' :: (Num t, Ord t) =>  [[a]] -> [a] -> t -> [[a]]
sdvig' reslist xs n 
    |n == 1 = reverse(reslist)
    |n > 1 = sdvig' ([sdviger xs]++reslist) (sdviger xs) (n-1)

sdviger xs = tail xs ++ [head xs]


-- 2 zadacha

koord :: (Ord a, Floating a) => (a,a) -> [(a,a)] -> a
koord (x,y) list = maximum (koord' (x,y) list)

koord' :: Floating a => (a,a) -> [(a,a)] -> [a]
koord' (x,y) list = [sqrt ((x - c)^2 + (y-b)^2) | (c,b) <- list]
{- (x,y) = (3,4)
    list = [(0,4),(3,0),(0,0)]
    koord (x,y) list = 5.0-}
    
    
-- 3 zadacha

destroyRepeaters list = destroyRepeaters' [] list (length list)
destroyRepeaters' reslist list n
    | n == 0 = reverse(reslist)
    | (head list) `notElem` reslist = destroyRepeaters' ([head list] ++ reslist) (tail list) (n-1)
    | (head list) `elem` reslist = destroyRepeaters' reslist (tail list) (n-1)
{- list = [1,1,2,3,3,3,3,4,5,5,6]
    destroyRepeaters list = [1,2,3,4,5,6]-}
    
   
-- 4 zadacha

solver a b eps f 
    |abs(f a) < eps = a
    |otherwise = solver (shag a b f) b eps f
shag a b f = a - ((f a) * (a-b))/((f a) - (f b))
{- a = 3 b = 8 eps = 1 f = x**3 - 18*x - 83
    solver a b eps f = 5.6952825533 -}
    
