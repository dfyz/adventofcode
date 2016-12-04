import Common

pointToDigit = digitFromTable rows
    where
        rows =
            ["  1  "
            ," 234 "
            ,"56789"
            ," ABC "
            ,"  D  "]

main = do
    ls <- lines <$> getContents
    let code = getCode (0, 2) pointToDigit ls
    putStrLn code
