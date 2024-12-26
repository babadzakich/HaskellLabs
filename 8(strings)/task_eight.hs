{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import GHC.IO.Encoding
import System.IO
import Text.Regex.PCRE.Light
import Text.Regex.Posix
instance {-# OVERLAPPING #-} Show String where
      show x = ['"'] ++ x ++ ['"']

--import GHC.Utils.Ppr (Style(lineLength))
third array = third' array 0 []
third' array acc string
    |null array = string
    |acc `mod` 3 == 2 = third' (tail array) 0 string ++ [head array]
    |otherwise = third' (tail array) (acc+1) string

{-extractUrls text = map (T.pack . fst) matches
  where
    -- The PCRE regex pattern
    pattern = "<a\\s+href\\s*=\\s*\"([^\"]+)\""
    matches = getAllMatches $ text =~ pattern
-}
extractSubheaders :: String -> [String]
extractSubheaders input =
  let regex = "<h[1-6]>.*?<\\/h[1-6]>" :: String
      matches = input =~ regex :: AllTextMatches [] String
      tags = getAllTextMatches matches
      subheaders = map extractInnerText (filter isSubheader tags)
  in subheaders

extractInnerText :: String -> String
extractInnerText tag =
  let regex = "<.*?>" :: String
      innerText = tag =~ regex :: (String, String, String)
  in snd3 innerText

isSubheader :: String -> Bool
isSubheader tag =
  let regex = "<h[1-6]>.*?<\\/h[1-6]>" :: String
  in tag =~ regex

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

main :: IO()
main = do
    file <- readFile "L1.html"
    --1 zadacha
    let count = length (filter (=='<') file)
    print count
    --2 zadacha
    let lns = lines file
    let uta = third lns
    writeFile "zadachaTwo.txt" (unlines uta)
    --zadacha 3
    inputFile <- openFile "L1.html" ReadMode
    outputFile <- openFile "zadachaThree.txt" WriteMode
    inputContent <- hGetContents inputFile
    let pattern = "<a href=\"(https?://[^\"]+)\""
        matches = inputContent =~ pattern :: [[String]]
    mapM_ (hPutStrLn outputFile . head . tail) matches
    hClose outputFile
    hClose inputFile
    --zadacha 4
    let hshki = extractSubheaders file
    writeFile "zadachaFour.txt" (unlines hshki)
    --zadacha 5
    headers <- openFile "zadachaFour.txt" ReadMode

    cp <- mkTextEncoding "cp866"
    
    writehandle <- openFile "zadachaFive.txt" WriteMode
    hSetEncoding writehandle cp
    contents <- hGetContents headers

    hPutStr writehandle (map toUpper contents)
    hClose headers
    hClose writehandle


