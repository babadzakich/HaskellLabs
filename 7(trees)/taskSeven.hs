import Data.Tree
data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
   deriving (Show, Read)

-- zadacha 0
{-treeBuilder list
    | null (tail list) == True = ABranch (ALeaf (head list)) (head list) (ALeaf (head list))
    |otherwise = ABranch (treeBuilder (tail list)) (head list) (treeBuilder (tail (tail list)))-}
tree = ABranch (ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1 (ABranch (ALeaf 34) 3 (ALeaf 35))

fringe :: ATree a -> [a]
fringe (ALeaf x) = [x]
fringe (ABranch left x right) = fringe left ++ [x] ++ fringe right

--zadacha 1
summary :: Num a => ATree a -> a
summary (ABranch left x right) = sum (fringe (ABranch left x right))
{-summary tree = 335-}

--zadacha 2
chetn list = filter even list
{-chetn (fringe tree) = [2,62,66,22,46,34] -}

--zadacha 3
paroobraznie (ABranch _ x _ ) (ABranch _ y _) = (x,y) 
paroobraznie (ALeaf x) (ABranch _ y _) = (x,y) 
paroobraznie (ABranch _ x _) (ALeaf y) = (x,y) 
paroobraznie (ALeaf x) (ALeaf y) = (x,y)

paroobrazovatel (ALeaf z) = []
paroobrazovatel (ABranch left x right) = [] ++ paroobrazovatel left ++ paroobrazovatel right  ++ [paroobraznie left right]
{- paroobrazovatel tree = [(62,66),(43,46),(21,22),(34,35),(2,3)] -}

--zadacha 4

resList = []
podderevo (ALeaf x) = resList ++ [((ALeaf x))]
podderevo (ABranch left x right) = resList ++ (podderevo left) ++ (podderevo right) ++ [(ABranch left x right)]
{- podderevo tree = [ALeaf 21,ALeaf 62,ALeaf 66,ABranch (ALeaf 62) 43 (ALeaf 66),ALeaf 46,ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46),
                     ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46)),ALeaf 34,ALeaf 35,ABranch (ALeaf 34) 3 (ALeaf 35),
                     ABranch (ABranch (ALeaf 21) 2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1 (ABranch (ALeaf 34) 3 (ALeaf 35))]
                    
-}

--zadacha 5
ravenstvo (ALeaf _) (ABranch _ _ _) = False
ravenstvo (ABranch _ _ _) (ALeaf _) = False
ravenstvo (ALeaf x) (ALeaf y) = (x==y)
ravenstvo (ABranch left1 x right1) (ABranch left2 y right2) = (x==y) && ((ravenstvo left1 left2 && ravenstvo right1 right2) || (ravenstvo left1 right2 && ravenstvo right1 left2)) 

--zadacha 6

instance (Eq a) => Eq (ATree a) where
  x == y = ravenstvo x y