type Id = Int

data Node a = Node Id a deriving(Eq, Show, Read)

data Tree a = Leaf (Node a) | Branch (Tree a) (Node a) (Tree a)
    deriving (Eq, Show, Read)
data Vertex a = Vertex {idv :: Id, val :: a, parent,left,right :: Maybe Id}
    deriving (Eq,Show,Read)
data Tr2w a = Tr2w {root :: Id, vs :: [Vertex a]}
    deriving (Eq,Show,Read)

sample :: Tr2w Int
sample = Tr2w 1
  [ Vertex 1 1 Nothing (Just 2) (Just 3)
  , Vertex 2 2 (Just 1) (Just 21) (Just 22)
  , Vertex 3 3 (Just 1) (Just 34) (Just 35)
  , Vertex 21 21 (Just 2) Nothing Nothing
  , Vertex 22 22 (Just 2) (Just 43) (Just 46)
  , Vertex 34 34 (Just 3) Nothing Nothing
  , Vertex 35 35 (Just 3) Nothing Nothing
  , Vertex 43 43 (Just 22) (Just 62) (Just 66)
  , Vertex 46 46 (Just 22) Nothing Nothing
  , Vertex 62 62 (Just 3) Nothing Nothing
  , Vertex 66 66 (Just 3) Nothing Nothing
  ]


-- pervoe zadanie
getNodebyIdInTr2w :: Tr2w a -> Id -> Maybe (Vertex a)
getNodebyIdInTr2w tree targetId = findVertexById targetId (vs tree)
-- getNodebyIdInTr2w sample 2 = Just (Vertex {idv = 2, val = 2, parent = Just 1, left = Just 21, right = Just 22})
findVertexById :: Id -> [Vertex a] -> Maybe (Vertex a)
findVertexById _ [] = Nothing
findVertexById targetId (v:vs)
  | idv v == targetId = Just v
  | otherwise = findVertexById targetId vs

-- vtoroe zadanie
mapTr2w :: (a -> a) -> Tr2w a -> Tr2w a
mapTr2w f (Tr2w rootId vertices) = Tr2w rootId (map (mapVertex f) vertices)

mapVertex :: (a -> a) -> Vertex a -> Vertex a
mapVertex f (Vertex vertexId value parentId leftChildId rightChildId) =
  Vertex vertexId (f value) parentId leftChildId rightChildId
{-f x = x^2
mapTr2w f sample = 
Tr2w {root = 1, vs = [Vertex {idv = 1, val = 1, parent = Nothing, left = Just 2, right = Just 3},
Vertex {idv = 2, val = 4, parent = Just 1, left = Just 21, right = Just 22},
Vertex {idv = 3, val = 9, parent = Just 1, left = Just 34, right = Just 35},
Vertex {idv = 21, val = 441, parent = Just 2, left = Nothing, right = Nothing},
Vertex {idv = 22, val = 484, parent = Just 2, left = Just 43, right = Just 46},
Vertex {idv = 34, val = 1156, parent = Just 3, left = Nothing, right = Nothing},
Vertex {idv = 35, val = 1225, parent = Just 3, left = Nothing, right = Nothing},
Vertex {idv = 43, val = 1849, parent = Just 22, left = Just 62, right = Just 66},
Vertex {idv = 46, val = 2116, parent = Just 22, left = Nothing, right = Nothing},
Vertex {idv = 62, val = 3844, parent = Just 3, left = Nothing, right = Nothing},
Vertex {idv = 66, val = 4356, parent = Just 3, left = Nothing, right = Nothing}]}
-}

--tretie zadanie
tr2wToTree :: Tr2w a -> Tree a
tr2wToTree (Tr2w rootId vertices) = constructor rootId vertices
  where
    constructor :: Id -> [Vertex a] -> Tree a
    constructor id vertices =
        let vertex = findVertexById2 id vertices
            children = getChildrenIds vertex
            leftChild = case leftt children of
                Just leftId -> constructor leftId vertices
                Nothing -> Leaf (convertToNode vertex)
            rightChild = case rightt children of
                Just rightId -> constructor rightId vertices
                Nothing -> Leaf (convertToNode vertex)
        in Branch leftChild (convertToNode vertex) rightChild


    findVertexById2 :: Id -> [Vertex a] -> Vertex a
    findVertexById2 _ [] = error "Vertex not found"
    findVertexById2 targetId (v:vs)
        | idv v == targetId = v
        | otherwise = findVertexById2 targetId vs

    getChildrenIds :: Vertex a -> (Maybe Id, Maybe Id)
    getChildrenIds (Vertex _ _ _ leftId rightId) = (leftId, rightId)

    convertToNode :: Vertex a -> Node a
    convertToNode (Vertex id' value _ _ _) = Node id' value

leftt :: (Maybe a, Maybe b) -> Maybe a
leftt (leftId, _) = leftId

rightt :: (Maybe a, Maybe b) -> Maybe b
rightt (_, rightId) = rightId
{- tr2wToTree sample = Branch (Branch (Branch (Leaf (Node 21 21)) (Node 21 21) (Leaf (Node 21 21))) 
(Node 2 2) (Branch (Branch (Branch (Leaf (Node 62 62)) (Node 62 62) (Leaf (Node 62 62))) (Node 43 43) (Branch (Leaf (Node 66 66)) 
(Node 66 66) (Leaf (Node 66 66)))) (Node 22 22) (Branch (Leaf (Node 46 46)) (Node 46 46) (Leaf (Node 46 46))))) 
(Node 1 1) (Branch (Branch (Leaf (Node 34 34)) (Node 34 34) (Leaf (Node 34 34))) (Node 3 3) (Branch (Leaf (Node 35 35)) 
(Node 35 35) (Leaf (Node 35 35))))
-}


treeToTr2w :: Tree a -> Tr2w a
treeToTr2w tree = Tr2w rootId vertices
  where
    (rootId, vertices) = convertToTr2w tree 0

    convertToTr2w :: Tree a -> Id -> (Id, [Vertex a])
    convertToTr2w (Leaf node) parentId =
      let vertex = convertToVertex node parentId Nothing Nothing
      in (getId node, [vertex])

    convertToTr2w (Branch leftChild node rightChild) parentId =
      let (leftId, leftVertices) = convertToTr2w leftChild (parentId + 1)
          (rightId, rightVertices) = convertToTr2w rightChild (parentId + 2)
          nodeVertex = convertToVertex node parentId (Just leftId) (Just rightId)
      in (parentId, nodeVertex : leftVertices ++ rightVertices)

    convertToVertex :: Node a -> Id -> Maybe Id -> Maybe Id -> Vertex a
    convertToVertex (Node id' value) parentId leftId rightId = Vertex id' value (Just parentId) leftId rightId

    getId :: Node a -> Id
    getId (Node id' _) = id'
    --treeNew = tr2wToTree sample
{- treeToTr2w treeNew = Tr2w {root = 1, 
vs = [Vertex {idv = 1, val = 1, parent = Nothing, left = Just 2, right = Just 3},
Vertex {idv = 2, val = 2, parent = Nothing, left = Just 21, right = Just 22},
Vertex {idv = 21, val = 21, parent = Nothing, left = Just 21, right = Just 21},
Vertex {idv = 21, val = 21, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 21, val = 21, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 22, val = 22, parent = Nothing, left = Just 43, right = Just 46},
Vertex {idv = 43, val = 43, parent = Nothing, left = Just 62, right = Just 66},
Vertex {idv = 62, val = 62, parent = Nothing, left = Just 62, right = Just 62},
Vertex {idv = 62, val = 62, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 62, val = 62, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 66, val = 66, parent = Nothing, left = Just 66, right = Just 66},
Vertex {idv = 66, val = 66, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 66, val = 66, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 46, val = 46, parent = Nothing, left = Just 46, right = Just 46},
Vertex {idv = 46, val = 46, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 46, val = 46, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 3, val = 3, parent = Nothing, left = Just 34, right = Just 35},
Vertex {idv = 34, val = 34, parent = Nothing, left = Just 34, right = Just 34},
Vertex {idv = 34, val = 34, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 34, val = 34, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 35, val = 35, parent = Nothing, left = Just 35, right = Just 35},
Vertex {idv = 35, val = 35, parent = Nothing, left = Nothing, right = Nothing},
Vertex {idv = 35, val = 35, parent = Nothing, left = Nothing, right = Nothing}]}
я не понял в каком месте у меня пошла ошибка, в первом или втором конверторе
-}

-- Четвертое задание
merge :: Tr2w a -> Tr2w a -> Tr2w a
merge (Tr2w root1 vertices1) (Tr2w root2 vertices2) =
    let newRoot = max root1 root2
        updatedVertices2 = map (\(Vertex idv val p l r) -> Vertex (idv + root1) val p (addOffset l) (addOffset r)) vertices2
        addOffset (Just x) = Just (x + root1)
        addOffset Nothing = Nothing
    in Tr2w newRoot (vertices1 ++ updatedVertices2)


tree1 :: Tr2w Int
tree1 = Tr2w 1
  [ Vertex 1 10 (Just 2) (Just 3) Nothing
  , Vertex 2 20 (Just 4) (Just 5) Nothing
  , Vertex 3 30 (Just 6) (Just 7) Nothing
  , Vertex 4 40 Nothing Nothing Nothing
  , Vertex 5 50 Nothing Nothing Nothing
  , Vertex 6 60 Nothing Nothing Nothing
  , Vertex 7 70 Nothing Nothing Nothing
  ]

tree2 :: Tr2w Int
tree2 = Tr2w 8
  [ Vertex 8 80 (Just 9) (Just 10) Nothing
  , Vertex 9 90 (Just 11) (Just 12) Nothing
  , Vertex 10 100 (Just 13) (Just 14) Nothing
  , Vertex 11 110 Nothing Nothing Nothing
  , Vertex 12 120 Nothing Nothing Nothing
  , Vertex 13 130 Nothing Nothing Nothing
  , Vertex 14 140 Nothing Nothing Nothing
  ]
  {-
  Tr2w {root = 8, vs = [
    Vertex {idv = 1, val = 10, parent = Just 2, left = Just 3, right = Nothing},
    Vertex {idv = 2, val = 20, parent = Just 4, left = Just 5, right = Nothing},
    Vertex {idv = 3, val = 30, parent = Just 6, left = Just 7, right = Nothing},
    Vertex {idv = 4, val = 40, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 5, val = 50, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 6, val = 60, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 7, val = 70, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 9, val = 80, parent = Just 9, left = Just 11, right = Nothing},
    Vertex {idv = 10, val = 90, parent = Just 11, left = Just 13, right = Nothing},
    Vertex {idv = 11, val = 100, parent = Just 13, left = Just 15, right = Nothing},
    Vertex {idv = 12, val = 110, parenaddTr2w :: Tr2w a -> Id -> Tr2w a -> Maybe (Tr2w a)
addTr2w mainTree idToAdd treeToAdd = do
  vertexToAdd <- findVertexById idToAdd (vs mainTree)
  let newRootId = maximum (root mainTree : root treeToAdd : map idv (vs mainTree)) + 1
      updatedTreeToAdd = updateIds (root treeToAdd) newRootId (vs treeToAdd)
      updatedVertices = vertexToAdd { right = Just newRootId } : vs mainTree ++ updatedTreeToAdd
  return $ Tr2w newRootId updatedVertices

findVertexById :: Id -> [Vertex a] -> Maybe (Vertex a)
findVertexById _ [] = Nothing
findVertexById id (v:vs)
  | idv v == id = Just v
  | otherwise = findVertexById id vs

updateIds :: Id -> Id -> [Vertex a] -> [Vertex a]
updateIds oldId newId vertices =
  map updateVertex vertices
  where
    updateVertex v@(Vertex id value left right parent) =
      v { idv = if id == oldId then newId else id
        , left = if left == Just oldId then Just newId else left
        , right = if right == Just oldId then Just newId else right
        , parent = if parent == Just oldId then Just newId else parent
        }
t = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 13, val = 120, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 14, val = 130, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 15, val = 140, parent = Nothing, left = Nothing, right = Nothing}]}
  -}


-- задача 5
addTr2w :: Tr2w a -> Id -> Tr2w a -> Maybe (Tr2w a)
addTr2w mainTree idToAdd treeToAdd = do
  vertexToAdd <- findVertexById idToAdd (vs mainTree)
  let newRootId = maximum (root mainTree : root treeToAdd : map idv (vs mainTree)) + 1
      updatedTreeToAdd = updateIds (root treeToAdd) newRootId (vs treeToAdd)
      updatedVertices = vertexToAdd { right = Just newRootId } : vs mainTree ++ updatedTreeToAdd
  return $ Tr2w newRootId updatedVertices

updateIds :: Id -> Id -> [Vertex a] -> [Vertex a]
updateIds oldId newId vertices = map updateVertex vertices
  where
    updateVertex v@(Vertex id value left right parent) =
      v { idv = if id == oldId then newId else id
        , left = if left == Just oldId then Just newId else left
        , right = if right == Just oldId then Just newId else right
        , parent = if parent == Just oldId then Just newId else parent
        }

mainTree = Tr2w 1
  [ Vertex 1 10 (Just 2) (Just 3) Nothing
  , Vertex 2 20 (Just 4) (Just 5) Nothing
  , Vertex 3 30 (Just 6) (Just 7) Nothing
  , Vertex 4 40 Nothing Nothing Nothing
  , Vertex 5 50 Nothing Nothing Nothing
  , Vertex 6 60 Nothing Nothing Nothing
  , Vertex 7 70 Nothing Nothing Nothing
  ]

treeToAdd = Tr2w 8
  [ Vertex 8 80 (Just 9) (Just 10) Nothing
  , Vertex 9 90 (Just 11) (Just 12) Nothing
  , Vertex 10 100 (Just 13) (Just 14) Nothing
  , Vertex 11 110 Nothing Nothing Nothing
  , Vertex 12 120 Nothing Nothing Nothing
  , Vertex 13 130 Nothing Nothing Nothing
  , Vertex 14 140 Nothing Nothing Nothing
  ]

{-addTr2w mainTree 2 treeToAdd
Just (Tr2w {root = 9, vs = 
    [Vertex {idv = 2, val = 20, parent = Just 4, left = Just 5, right = Just 9},
    Vertex {idv = 1, val = 10, parent = Just 2, left = Just 3, right = Nothing},
    Vertex {idv = 2, val = 20, parent = Just 4, left = Just 5, right = Nothing},
    Vertex {idv = 3, val = 30, parent = Just 6, left = Just 7, right = Nothing},
    Vertex {idv = 4, val = 40, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 5, val = 50, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 6, val = 60, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 7, val = 70, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 9, val = 80, parent = Nothing, left = Just 9, right = Just 10},
    Vertex {idv = 9, val = 90, parent = Nothing, left = Just 11, right = Just 12},
    Vertex {idv = 10, val = 100, parent = Nothing, left = Just 13, right = Just 14},
    Vertex {idv = 11, val = 110, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 12, val = 120, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 13, val = 130, parent = Nothing, left = Nothing, right = Nothing},
    Vertex {idv = 14, val = 140, parent = Nothing, left = Nothing, right = Nothing}]})
-}