import Text.ParserCombinators.Parsec

data AV = N String | V String | AV :+: AV | AV :*: AV deriving (Read, Show, Eq)

av :: Parser AV
av = try avplus <|> try avmult <|> try variable <|> (N <$> nat)
  where
    variable = do
        char 'x'
        num <- nat
        return (V num)
    avplus = do
        char '('
        e1 <- av
        char '+'
        e2 <- av
        char ')'
        return (e1 :+: e2)
    avmult = do
        char '('
        e1 <- av
        char '*'
        e2 <- av
        char ')'
        return (e1 :*: e2)

nat :: Parser String
nat = many1 digit

eval :: AV -> [Int] -> Int
eval (N s) _ = read s
eval (V s) vars = let varIndex = read s - 1 in if varIndex < length vars then vars !! varIndex else error "Not enough values for variables"
eval (e1 :+: e2) vars = eval e1 vars + eval e2 vars
eval (e1 :*: e2) vars = eval e1 vars * eval e2 vars

run :: String -> [Int] -> Int
run input vars =
    case parse av "" input of
        Left err -> error $ show err
        Right ast -> eval ast vars

main :: IO ()
main = do
    print $ parse av "" "(((x1+x2)+x4)*2)"
    print $ run "(((x1+x2)+x3)*2)" [-4,5,6]
