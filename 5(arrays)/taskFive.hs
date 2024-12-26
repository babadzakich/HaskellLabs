import Data.Array
import Data.List
--arD :: Array(Double, Double)Double
arD = listArray (1,5) [((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0),(8.3,3.8)]
arI :: Array (Int,Int) Int
arI = array ((1,1),(2,3)) [((1,1),8), ((1,2),11), ((1,3),4),((2,1),3), ((2,2),2),  ((2,3),4)]

-- 1 zadacha
suma arr = sum arr
{-foldl (+) 0 arI, sum (elems arI)-}
{- suma arI = 32-}

--2 zadacha
sumaInd arr = sumaInd' arr (indices arr) 0
sumaInd' arr ind summary
    | null ind == True = summary
    | otherwise = sumaInd' arr (tail ind) (summary + (arr ! (head ind)))
{- sumaInd arI = 32-}

-- 3 zadacha
ravnElem arr = ravnElem' arr (indices arr) []
ravnElem' arr ind resList
    | null ind == True = reverse(resList)
    | checkRavn arr (head ind) == True = ravnElem' arr (tail ind) ([(arr ! (head ind))] ++ resList)
    | otherwise = ravnElem' arr (tail ind) resList
checkRavn arr ind
    | (arr ! ind) == (smma ind) = True
    | otherwise = False
smma (x,y) = x+y    
{- ravnElem arI = [4,3]-}

--4 zadacha
changeRavn arr = arr // [((a,b), 77) | a <- [1..2], b <- [1..3], checkRavn arr (a,b) == True]
{- changeRavn arI = ((1,1),(2,3)) [((1,1),8), ((1,2),11), ((1,3),77),((2,1),77), ((2,2),2),  ((2,3),4)]-}

--5 zadacha
lomLen arr = lomLen' arr (tail (indices arr)) 0 (head (indices arr)) (head (indices arr))
lomLen' arr ind sma prev curr
    |null ind == True = sma
    |otherwise = lomLen' arr (tail ind) (sma + (findLen (arr ! (head ind)) (arr ! prev))) curr (head ind)
    
findLen (x,y) (x1,y1) = sqrt((x-x1)**2 +(y-y1)**2)
{- lomLen arD = 22514.996104438396
-}

-- 6 zadacha
a = listArray (1,10) ([1.3,1.2,1.9,1.25,1.4,0.1,1.2,1.32,1.23,1.5]::[Double])
usredn arr = [arr!(head(indices(arr)))] ++ [(((arr!(n-1)) + (arr!n) + (arr!(n+1)))/3)| n <- (indices arr), n /= (last(indices(arr))), n /= (head(indices(arr)))] ++ [arr!(last(indices(arr)))]
{-usredn arr = listArray bds [elem]
    where
        bds = bounds arr
        elem-}
     
{- usredn =  array (1,8) [(1,1.4666666666666668),(2,1.45),(3,1.5166666666666666),(4,0.9166666666666666),(5,0.9),(6,0.8733333333333334),(7,1.25),(8,1.3499999999999999)]
-}

