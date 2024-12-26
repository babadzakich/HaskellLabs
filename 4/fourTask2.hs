import Data.List
--list = [25 ,31, 55 ,2, 83, 0, 1, 4, 55], least = 3
least' list = filter (\x -> x `notElem` list)[0..maximum(list)]
least list = minimum(least' list)


sliyanie :: Ord a => [a] -> [a] -> [a]
sliyanie list1 list2 = sliyanie' list1 list2 []
sliyanie' :: Ord a => [a] -> [a] -> [a] -> [a]
sliyanie' list1 list2 resList
    |(null list1) == True && (null list2) == True = reverse(resList)
    |(null list1)==True && (null list2) ==False = sliyanie' list1 (tail list2) ([head(list2)]++resList)
    |(null list1) ==False && (null list2) ==True = sliyanie' (tail list1) list2 ([head(list1)]++resList)
    |(head(list1) > head(list2)) = sliyanie' list1 (tail list2) ([head(list2)]++resList)
    |(head(list1) <= head(list2)) = sliyanie' (tail list1) list2 ([head(list1)]++resList)
{- list1 = [10, 22, 33, 165]
    list2 = [3, 15, 49, 122, 144, 150, 200]
    sliyanie list1 list2 =  [3, 10, 15, 22, 33, 49, 142, 144, 150, 165, 200]-}

repeatNumber :: (Foldable t, Eq b, Num b) => b -> t b -> b
repeatNumber number list = foldl (\acc x -> if x == number then acc + 1 else acc) 0 list
{-list = [1, 2, 3, 2, 1, 2, 3, 4,1,1] number = 1
repeatNumber 1 list = 4-}

majorNumberFunc :: [Int] -> Int
majorNumberFunc list = majorNumberFunc' list (nub list)    

majorNumberFunc' :: Foldable t => t Int -> [Int] -> Int
majorNumberFunc' list set 
    | null set == True = -1
    | 2* (repeatNumber (head set) list) >  length list = (head set)
    | otherwise = majorNumberFunc' list (tail set)
{- list = [1,1,1,1,2,3]
majorNumberFunc list (nub list) 0 = 1-}

frequencyFunc :: (Eq b, Num b) => [b] -> [(b,b)]
frequencyFunc list = frequencyFunc' list (nub list) []

frequencyFunc' :: (Foldable t, Eq b, Num b) => t b -> [b] -> [(b,b)] -> [(b,b)]
frequencyFunc' list set resList
    |null set == True = reverse(resList)
    |otherwise = frequencyFunc' list (tail set) ([((head set),(repeatNumber (head set) list))]++resList)