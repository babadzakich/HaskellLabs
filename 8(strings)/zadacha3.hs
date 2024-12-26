import Text.Regex.Posix
import System.IO

main = do
    -- Открываем входной и выходной файлы
    inputFile <- openFile "L1.html" ReadMode
    outputFile <- openFile "output.txt" WriteMode

    -- Читаем содержимое входного файла
    inputContents <- hGetContents inputFile

    -- Применяем регулярное выражение для поиска URL-адресов
    let pattern = "<a href=\"(https?://[^\"]+)\""
        matches = inputContents =~ pattern :: [[String]]

    -- Записываем найденные URL-адреса в выходной файл
    mapM_ (hPutStrLn outputFile . head . tail) matches

    -- Закрываем файлы
    hClose inputFile
    hClose outputFile