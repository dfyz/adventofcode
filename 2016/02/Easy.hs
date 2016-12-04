import Common

pointToDigit = digitFromTable rows
    where
        rows =
            ["123"
            ,"456"
            ,"789"]

main = do
    ls <- lines <$> getContents
    let code = getCode (1, 1) pointToDigit ls
    putStrLn code
