data Op = Low | High deriving Show

data BotOp = OpForBot {
    op :: Op,
    botId :: String
} deriving Show

data BotValue
    = FromInput Integer
    | FromOtherBot BotOp
    deriving Show

parseLines = (concatMap $ parseLine . tokens)
    where
        tokens = words
        parseOp "low" = Low
        parseOp "high" = High
        parseLine ["bot", x, "gives", op1, "to", y, y', "and", op2, "to", z, z'] =
            let otherBotId = formId "bot" x in
            [
                (formId y y', FromOtherBot $ OpForBot (parseOp op1) otherBotId),
                (formId z z', FromOtherBot $ OpForBot (parseOp op2) otherBotId)
            ]
        parseLine ["value", x, "goes", "to", y, y'] =
            [
                (formId y y', FromInput $ read x)
            ]
        formId f s = f ++ " " ++ s

evalVal descs valId = map evalSingle valDescs
    where
        valDescs = map snd $ filter ((valId ==) . fst) descs
        evalSingle (FromInput x) = x
        evalSingle (FromOtherBot (OpForBot op botId)) = evalOp op (evalVal descs botId)
        evalOp Low [a, b] = min a b
        evalOp High [a, b] = max a b

main = do
    descs <- parseLines <$> (lines <$> getContents)
    putStrLn $ show $ evalVal descs "bot 2"
    putStrLn $ show $ evalVal descs "bot 1"
    putStrLn $ show $ evalVal descs "bot 0"
    putStrLn $ show $ evalVal descs "output 0"
    putStrLn $ show $ evalVal descs "output 1"
    putStrLn $ show $ evalVal descs "output 2"
