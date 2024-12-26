import Data.List
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map

{-# LANGUAGE FlexibleInstances #-}
instance {-# OVERLAPPING #-} Show String where
    show x = ['"'] ++ x ++ ['"']

data Gender = F | M
                    deriving (Show,Eq, Ord)
listName = ["Оля", "Женя", "Катя", "Женя", "Паша", "Саша", "Саша", "Маша", "Надя", "Юля"] :: [String]
listID = [131, 132, 134, 135, 136, 137, 138, 139, 140, 141] :: [Int]
listGender = [F, F, F, M, M, M, F, F, F, F] :: [Gender]
listAge = [17, 18, 17, 19, 18, 19, 21, 19, 18, 20] :: [Int]
listHasFriends = [True, False, True, False, False, True, False, True, True, False] :: [Bool]

-- zadacha 1

--svyazan list1 list2 list3 list4 list5 = listArray ((head list1), (last list1)) [(list2 !! i, list3 !! i, list4 !! i, list5 !! i) | i <-[0..((length list1)-1)] ]
svyazan list1 list2 list3 list4 list5 = Map.fromList [((list1 !! i), ((list2!!i),(list3!!i),(list4!!i),(list5!!i)))| i <- [0..((length list1)-1)]]

{- svyazan listID listName listGender listAge listHasFriends = fromList [(131, ("Оля",F,17,True)),(132, ("Женя",F,18,False)),
                   (134, ("Катя",F,17,True)),(135, ("Женя",M,19,False)),(136, ("Паша",M,18,False)),(137, ("Саша",M,19,True)),
                   (138, ("Саша",F,21,False)),(139, ("Маша",F,19,True)),(140, ("Надя",F,18,True)),(141, ("Юля",F,20,False))]
-}

-- zadacha 2
showMale mp = zip (Map.keys (male mp)) (frst (Map.elems(male mp)))
male mp = Map.filter (ismale) mp 
ismale (_,a,_,_) | a == M = True
                 |otherwise = False
frst lst = [a | (a,_,_,_) <- lst]
{- я взял базу данных, которая получилась из mp = svyazan listID listName listGender listAge listHasFriends
    showMale mp = [(135, "Женя"), (136, "Паша"),(137, "Саша")]
-}

-- zadacha 3
frequency list = frequency' list (Map.empty)
frequency' lst mpp 
    |null lst == True = Map.toList mpp
    |Map.notMember (head lst) mpp == True = frequency' (tail lst) (Map.insert (head lst) 1 mpp)
    |otherwise = frequency' (tail lst) (Map.adjust (1 +) (head lst) mpp)
    
{- list = [1,2,3,3,4,2,2,1,4,1]
   frequency list = [(1,3),(2,3),(3,2),(4,2)]-}