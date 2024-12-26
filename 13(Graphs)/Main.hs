import System.IO
import Data.Array

main = do
    let orgr = array (1,6) [(1,[4,2]),(2,[5]),(3,[6,5]),(4,[2]),(5,[4]),(6,[6])]
        result = array (1,6) [0]
    print result