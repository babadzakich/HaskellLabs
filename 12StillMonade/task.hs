import Data.Maybe
import Control.Monad.Writer
import Control.Monad.State

data ATree a = ALeaf a | ABranch (ATree a) a (ATree a)
    deriving (Show, Read)
dTree :: ATree Double
-- dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 0 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))
dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))

--1 zadacha
divWithTreeM :: Double -> ATree Double -> Maybe Double
divWithTreeM number tree = divWithTreeM' number tree (dM tree)

divWithTreeM' number tree mult
    | isNothing mult = Nothing
    | otherwise = Just(number / fromJust mult)

dM :: ATree Double -> Maybe Double
dM (ALeaf a)
    | a == 0 = Nothing
    | otherwise = Just a
dM (ABranch x a y)
    |isNothing(dM x) = Nothing
    |isNothing(dM y) = Nothing
    | a == 0  = Nothing
    |otherwise = Just(fromJust(dM x) * a * fromJust(dM y))
--divWithTreeM 5 dTree = Just 1.4069862369930015e-13

idTree :: ATree (Int,Double)
idTree =
    ABranch
       (
         ABranch
            (ALeaf (4,21))
              (2,2.2)
            (
              ABranch
                 (
                   ABranch
                     (ALeaf (100,62))
                       (7,0)
                     (ALeaf (200,66))
                 )
                     (5,22)
                  (ALeaf (203,46))
            )

       )
             (1,1.1)
     (
       ABranch
         (ALeaf (301,34))
            (3,3.3)
         (ALeaf (307,0))
     )

--2 zadacha
divWithTreeE :: Double -> ATree (Int, Double) -> Either String Double
divWithTreeE n (ALeaf (id, x)) 
  | x /= 0 = Right (n / x) 
  |otherwise = Left ("Oshibka s ID: " ++ show id)
divWithTreeE n (ABranch leftTree (id, x) rightTree) =
  case divWithTreeE n leftTree of
    Left msg -> Left msg
    Right resultLeft ->
      case divWithTreeE n rightTree of
        Left msg -> Left msg
        Right resultRight ->
          if x /= 0
            then Right ((n / x) + resultLeft + resultRight)
            else Left ("Oshibka s ID: " ++ show id)
-- ghci> divWithTreeE 20 idTree
-- Left "Something went wrong with ID: 7"


--3 zadacha
sumWithTreeE :: ATree (Int, Double) -> Writer String Double
sumWithTreeE (ALeaf (id, x)) = do
  tell ("Processing leaf with id " ++ show id ++ "\n") 
  return x
sumWithTreeE (ABranch left (id, x) right) = do
  l <- sumWithTreeE left
  r <- sumWithTreeE right
  tell ("Processing branch with ID: " ++ show id ++ "\n") 
  return (x + l + r)
{--
ghci> sumWithTreeE idTree
WriterT (Identity (257.59999999999997,"Now is adding ID: 4\n
Now is adding ID: 100\n
Now is adding ID: 200\n
Now is adding ID: 7\n
Now is adding ID: 203\n
Now is adding ID: 5\n
Now is adding ID: 2\n
Now is adding ID: 301\n
Now is adding ID: 307\n
Now is adding ID: 3\n
Now is adding ID: 1\n"))
--}

--zadacha 4.1


numTree :: ATree Double -> ATree (Int, Double)
numTree tree = numTree' tree 1

numTree' (ALeaf x) n = ALeaf (n, x)
numTree' (ABranch left x right) n =
  let ln = numTree' left (2 * n)
      rn = numTree' right (2 * n + 1)
  in ABranch ln (n,x) rn
{--
ghci> numTree dTree
ABranch (ABranch (ALeaf (4,21.0)) (2,2.2) (ABranch 
(ABranch (ALeaf (20,62.0)) (10,43.0) (ALeaf (21,66.0))) (5,22.0) 
(ALeaf (11,46.0)))) (1,1.1) (ABranch (ALeaf (6,34.0)) (3,3.3) (ALeaf (7,35.0)))
--}

--zadacha 4.2

enumS :: ATree Double -> State Int (ATree (Int, Double))
enumS (ALeaf x) = do
  id <- get
  put (id + 1)
  return (ALeaf (id, x))
enumS (ABranch left x right) = do
  id <- get
  put (id + 1)
  left' <- enumS left
  right' <- enumS right
  return (ABranch left' (id, snd (rootLabel left')) right')

rootLabel :: ATree a -> a
rootLabel (ALeaf x) = x
rootLabel (ABranch _ x _) = x

-- Function to enumerate the nodes of a tree and add IDs to the values
numTree2 :: ATree Double -> ATree (Int, Double)
numTree2 tree = evalState (enumS tree) 0
{-
ghci> numTree2 dTree
ABranch (ABranch (ALeaf (2,21.0)) (1,21.0) 
(ABranch (ABranch (ALeaf (5,62.0)) (4,62.0) 
(ALeaf (6,66.0))) (3,62.0) (ALeaf (7,46.0)))) (0,21.0) 
(ABranch (ALeaf (9,34.0)) (8,34.0) (ALeaf (10,35.0)))
-}