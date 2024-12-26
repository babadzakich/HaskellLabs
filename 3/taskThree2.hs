allList :: Eq a => [a] -> Bool
allList xs = all (== head(xs)) xs
{-xs = [1,1,1,1] allList xs = True, xs = [1,1,2] allList xs = False-}

pifC :: [(Int,Int,Int)]
pifC = pifC' (\(x,y,z) -> x>0 && y>0 && z>0)
pifC' :: ((Int , Int , Int) -> Bool) -> [(Int,Int,Int)]
pifC' cond = filter cond [((m^2-n^2),2*m*n, m^2+n^2) | n <- [1..], m <- [n+1,n+3..]]
-- примитивные пифагоровы тройки
{- take 5 pifC = [(3,4,5),(15,8,17),(35,12,37),(63,16,65),(99,20,101)]-}

pif :: [(Integer , Integer , Integer)]
pif = [(a, b, c) |c <- [1..], a <- [1..c], b <- [1..c], c^2 == a^2 + b^2, a<b, b<c]
-- обычные пифагоровы тройки
{-take 5 pif = [(3,4,5), (6,8,10),(5,12,13),(9,12,15),(8,15,17)-}

soversh = filter (\ n -> sum(delitel n) == n)[1..]
delitel c = [ a |a <- [1..c `div` 2], mod c a == 0]
{- take 5 soversh = [6, 28, 496, 8128]-}

kantor = [(a,b) |c <- [2..], a <- [1..c-1], b <- [1..c-1], a+b==c]
{- take 5 kantor = [(1,1),(1,2),(2,1),(1,3),(2,2)]-}

maxLength :: (Ord a, Floating a) => [(a,a)] -> a
maxLength list = maximum(maxLength' list)
maxLength' :: Floating a => [(a,a)] -> [a]
maxLength' list = [ sqrt((x-z)^2+(y-w)^2) | (x,y) <- list, (z,w) <- list]
{- list = [((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0)], maxLength list = 11248.134877076287-}