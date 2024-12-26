import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Token (makeTokenParser)
import Text.Parsec.Error

data AV = N String | AV :+: AV | AV :*: AV deriving (Read,Show,Eq)

languageDef = haskellStyle

lexer = makeTokenParser languageDef

fstdig :: Parser Char
fstdig = oneOf "123456789"

dig :: Parser Char
dig = oneOf "0123456789"

nat :: Parser String
nat = do
    first <- fstdig
    rest <- many dig
    return (first:rest)

avplus :: Parser AV
avplus = do
    char '('
    e1 <- av
    char '+'
    e2 <- av
    char ')'
    return (e1 :+: e2)

avmult :: Parser AV
avmult = do
    char '('
    e1 <- av
    char '*'
    e2 <- av
    char ')'
    return (e1 :*: e2)

av :: Parser AV
av = try avplus <|> try avmult <|> (N <$> nat)

parser :: String -> Either ParseError AV
parser input = parse av "" input

eval :: AV -> Int
eval (N s) = read s
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

run :: String -> Int
run input =
    case parser input of
        Left err -> error $ show err
        Right ast -> eval ast

main :: IO ()
main = do
    print $ parser "51"
    print $ parser "051"
    print $ parser "-51"
    print $ parser "5-1"
    print $ parser "((1+2)*3)"
    print $ parser "((1+2)*3))"
    print $ eval (N "51")
    print $ eval ((N "1" :+: N "2") :*: N "3")
    print $ run "51"
    print $ run "5-1"
    print $ run "((1+2)*3)"
    print $ run "((1+2)*3))"
{--
Right (N "51")
Left (line 1, column 1):
unexpected "0"
expecting "("
Left (line 1, column 1):
unexpected "-"
expecting "("
Right (N "5")
Right ((N "1" :+: N "2") :*: N "3")
Right ((N "1" :+: N "2") :*: N "3")
51
9
51
5
9
9
--}