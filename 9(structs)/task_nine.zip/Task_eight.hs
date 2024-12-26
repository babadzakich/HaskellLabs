import System.IO
import GHC.Tc.Types (WhereFrom)
import Data.Maybe (fromJust, isJust)
data Person = Person{
    name :: String,
    age :: Int,
    weight :: Float
} deriving(Eq)

instance Show Person where
    show (Person a b c) = 
        "name = " ++ a ++ "\n" ++
        "age = " ++ show b ++ "\n" ++
        "weight = " ++ show c

instance Read Person where
  readsPrec _ input =
    let [nameLine, ageLine, weightLine] = lines input
        name = drop (length "name = ") nameLine
        age = read (drop (length "age = ") ageLine)
        weight = read (drop (length "weight = ") weightLine)
    in [(Person name age weight, "")]
        
main = do
    --zadacha 1,2,3
    let p1 = Person "Beelzebub" 666 322.228
    writeFile "personShow.txt" (show p1)
    person <- readPersonFromFile "PersonRead.txt"
    print person



readPersonFromFile :: FilePath -> IO Person
readPersonFromFile filepath = do
  content <- readFile filepath
  case reads content :: [(Person, String)] of
    [(person, "")] -> return person
    _ -> error "Invalid data format."