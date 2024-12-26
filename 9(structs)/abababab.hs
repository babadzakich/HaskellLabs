import Control.Exception (catch, SomeException)

data Person = Person {
  name :: String,
  age :: Int,
  weight :: Float
} deriving (Read, Show)  -- This will automatically derive both Read and Show instances.


readPerson :: String -> Maybe Person
readPerson str = case reads str of
  [(person, "")] -> Just person
  _              -> Nothing

readPeopleFromFile :: FilePath -> IO [Maybe Person]
readPeopleFromFile filePath = do
  content <- readFile filePath
  return $ map readPerson (lines content)

main :: IO ()
main = do
  people <- readPeopleFromFile "PersonRead.txt"
  -- You can choose to either handle the Nothing case or filter it out
  let validPeople = map fromJust $ filter isJust people
  print validPeople
