import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Time

type SkipTable = HashMap Char Int


base :: Int
base = 1024

prime :: Int
prime = 13

rollingHash :: String -> Int -> Int
rollingHash str len = foldl' (\acc c -> (base * acc + fromEnum c) `mod` prime) 0 (take len str)

updateHash :: Int -> Char -> Char -> Int -> Int
updateHash prevHash oldChar newChar len =
  let oldCharVal = fromEnum oldChar
      newCharVal = fromEnum newChar
      removed = (prevHash - oldCharVal * base^(len - 1)) `mod` prime
  in (base * removed + newCharVal) `mod` prime


rkSearch :: String -> String -> [Int]
rkSearch pattern text =
  let patLen = length pattern
      txtLen = length text
      patHash = rollingHash pattern patLen
      firstHash = rollingHash (take patLen text) patLen

      loop acc i
        | i + patLen > txtLen = reverse acc
        | rollingHash (take patLen (drop i text)) patLen == patHash =
            if take patLen (drop i text) == pattern
              then loop (i : acc) (i + 1)
              else loop acc (i + 1)
        | otherwise = loop acc (i + 1)

  in if txtLen >= patLen then loop [] 0 else []


findAllNeedles :: String -> String -> [Int]
findAllNeedles haystack needle = go 0
  where
    n = length needle
    go i
      | i > length haystack - n = []
      | isPrefixOf' needle (drop i haystack) = i : go (i + 1)
      | otherwise = go (i + 1)

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- Вычисление префиксной функции для шаблона
prefixFunction :: Eq a => [a] -> [Int]
prefixFunction pattern = let
    n = length pattern
    go _ [] = []
    go k xs
        | k > 0 && head xs == pattern !! k = (k + 1) : go (k + 1) (tail xs)
        | k > 0 = go (findPrefixIndex k pattern) xs
        | otherwise = if head xs == pattern !! 0 then 1 : go 1 (tail xs) else 0 : go 0 (tail xs)
    findPrefixIndex i p = if i <= 0 || head p /= p !! i then 0 else i
    in go 0 pattern


-- Поиск всех вхождений подстроки в строке с использованием алгоритма Кнута-Морриса-Пратта
kmpSearchAll :: Eq a => [a] -> [a] -> [Int]
kmpSearchAll _ [] = []
kmpSearchAll needle haystack = let
    p = prefixFunction needle
    n = length needle
    m = length haystack
    go q s
        | q >= m = []
        | take n s == needle = q : go (q + (if n == 1 then 1 else if n `mod` 2 == 0 then n-1 else n-2)) (drop 1 s)
        | otherwise = go (q+1) (tail s)
    in go 0 haystack


createSkipTable :: String -> SkipTable
createSkipTable pattern = HashMap.fromList $ zip pattern [length pattern - 1, length pattern - 2 .. 0]

boyerMooreSearch :: String -> String -> Maybe [Int]
boyerMooreSearch text pattern
    | null text || null pattern = Nothing
    | length pattern > length text = Nothing
    | otherwise =
        let n = length text
            m = length pattern
            skipTable = createSkipTable pattern
            loop shift indexes
                | shift > n - m = Just indexes
                | otherwise =
                    let match = [i | i <- [0 .. m-1], pattern !! i /= text !! (i + shift)]
                    in if null match
                        then loop (shift + 1) (shift:indexes)
                        else loop (shift + (max 1 (HashMap.lookupDefault 0 (text !! (m-1)) skipTable))) indexes
        in loop 0 []


main :: IO ()
main = do

    text <- .
    if null text
        then putStrLn "Input text cannot be empty."
        else do
             pattern <- getLine
             if null pattern
                   then putStrLn "Pattern cannot be empty."
                   else do
                   start1 <- getCurrentTime
                   let result = boyerMooreSearch text pattern
                   end1 <- getCurrentTime
                   start2 <- getCurrentTime
                   let indices = kmpSearchAll pattern text
                   end2 <- getCurrentTime
                   start3<- getCurrentTime
                   let need = findAllNeedles text pattern
                   end3 <- getCurrentTime
                   start4 <- getCurrentTime
                   let h = rkSearch pattern text
                   end4 <-  getCurrentTime
                   if result == Just []
                         then putStrLn "Pattern not found"
                         else do
                              putStrLn $ "Время выполнения функции: " ++ show (diffUTCTime end1 start1)
                              putStrLn $ "Время выполнения функции: " ++ show (diffUTCTime end4 start4)
                              putStrLn $ "Время выполнения функции: " ++ show (diffUTCTime end3 start3)
                              putStrLn $ "Время выполнения функции: " ++ show (diffUTCTime end2 start2)
                              case result of
                                    Just indexes -> do
                                        --let n = length text
                                        --let reversedIndexes = [if length pattern > 1 then n - i - 2 else n - i - 1| i <- indexes]
                                        putStrLn $ "Pattern found at indexes using BM: " ++ show indexes
                                        putStrLn $ "Pattern found at indices using RK: " ++ show h
                                        putStrLn $ "Pattern found at indices using N2: " ++ show need
                                        putStrLn $ "Pattern found at indices using KMP: " ++ show indices
                                    Nothing -> putStrLn "Pattern not found"
