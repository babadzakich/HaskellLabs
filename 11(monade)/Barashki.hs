import Barans
import Data.Maybe


fatherOfMother :: Sheep -> Maybe Sheep
fatherOfMother sheepName = father (fromMaybe "bezMami" (mother sheepName))

fatherOfFatherOfMother :: Sheep -> Maybe Sheep
fatherOfFatherOfMother sheepName = father(fromMaybe "bezDedniy" (fatherOfMother sheepName))

parents :: Sheep -> [Sheep]
parents sheep = maybeToList (father sheep) ++ maybeToList(mother sheep) 

grandparent sheep = grandparent' (parents sheep) []
grandparent' par grandparents
    |null par = return grandparents
    |otherwise = grandparent' (tail par) (grandparents ++ parents(head par))
    
sirota sheep 
    | null(parents sheep) = True
    | otherwise = False

selekciya = ["i3", "i5", "i6", "i9", "i12"]

selectFather sheep
    | elem (fromJust (father sheep)) selekciya = Just(father sheep)
    | otherwise = Nothing

blizhayshiy Nothing = Nothing
blizhayshiy (Just parent)
            | elem parent selekciya = Just parent
            | otherwise = blizhayshiy (father parent)
