import Data.Array
import Data.List
import Data.Graph

data Queue a = Queue [a] [a]
newQueue = Queue [] []

emptyQueue (Queue [] []) = True
emptyQueue _ = False

enqueue (Queue xs ys) y = Queue xs (y++ys)

dequeue (Queue [] []) = error "Can't deq from an empty queue"
dequeue (Queue xs ys) = (head xs, Queue (tail xs) ys)
dequeue (Queue [] ys) = dequeue (Queue (reverse ys) [])
getStack (Queue xs ys) = Queue (tail xs) ys
getNode (Queue xs ys) = head xs
--zadacha 1
orgr = buildG (1, 6) [(1,2), (1,4), (4,2), (2,5), (5,4), (3,5), (3,6), (6,6)]  
--orgr = array (1,6) [(1,[4,2]),(2,[5]),(3,[6,5]),(4,[2]),(5,[4]),(6,[6])]

--zadacha 2

bfs :: Graph -> Int -> [Int]
bfs graph start = bfs' (enqueue start emptyQueue) (listArray (0, n) (repeat False)) []
  where
    n = snd (bounds graph)
    bfs' q visited res =
      case dequeue q of
        Nothing -> reverse res
        Just (v, q') ->
          if visited ! v
            then bfs' q' visited res
            else
              let neighbors = graph ! v
                  newQ = foldl' (\acc x -> enqueue x acc) q' neighbors
                  newVisited = visited // [(v, True)]
                  newRes = v : res
              in bfs' newQ newVisited newRes

main :: IO ()
main = do
  let result = bfs orgr 3
  print result