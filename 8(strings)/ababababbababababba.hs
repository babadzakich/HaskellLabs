import Text.Regex.Posix
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Function to extract URLs from <a href="..."> tags
extractUrls :: T.Text -> [T.Text]
extractUrls input = nub $ input =~ "<a\\s+href=['\"]([^'\"]+)['\"]" :: [T.Text]

-- Function to write URLs to a file
writeUrlsToFile :: FilePath -> [T.Text] -> IO ()
writeUrlsToFile filePath urls = TIO.writeFile filePath $ T.unlines urls

main :: IO ()
main = do
  -- Read HTML content from a file (you can replace this with your own HTML input)
  htmlContent <- TIO.readFile "L1.html"

  -- Extract URLs from HTML content
  let urls = extractUrls htmlContent

  -- Write URLs to a file
  writeUrlsToFile "output.txt" urls