import Control.Applicative
import Data.Array
import Data.List
import Text.Trifecta

rows = 6
cols = 50

upTo n = take n [0..]

data Op
    = Rect { c :: Int, r :: Int }
    | RotateRow { r :: Int, d :: Int }
    | RotateCol { c :: Int, d :: Int }
    deriving Show

data LCD = LCD { screen :: (Array (Int, Int) Char) }

startLcd :: LCD
startLcd = LCD $ listArray ((0, 0), (rows - 1, cols - 1)) (repeat '.')

doOp :: LCD -> Op -> LCD
doOp (LCD lcd) op = case op of
    (Rect c r) ->
        LCD (lcd // updates)
            where
                updates = [((r', c'), '#') | r' <- upTo r, c' <- upTo c]
    (RotateRow r d) ->
        LCD (lcd // updates)
            where
                dc = (`mod` cols)
                updates = [((r, c'), lcd ! (r, dc (c' - d))) | c' <- upTo cols]
    (RotateCol c d) ->
        LCD (lcd // updates)
            where
                dr = (`mod` rows)
                updates = [((r', c), lcd ! (dr (r' - d), c)) | r' <- upTo rows]


parseOp :: String -> Op
parseOp = fromSuccess . parseString p mempty
    where
        fromSuccess (Success x) = x
        parseInt = fromIntegral <$> decimal
        afterByDecimal = (string " by ") >> parseInt
        parseRect = (string "rect ") >> (Rect <$> parseInt <*> (char 'x' >> parseInt))
        parseRotateRow = (string "rotate row y=") >> (RotateRow <$> parseInt <*> afterByDecimal)
        parseRotateCol = (string "rotate column x=") >> (RotateCol <$> parseInt <*> afterByDecimal)
        p = parseRect <|> parseRotateRow <|> parseRotateCol

instance Show LCD where
    show lcd = intercalate "\n" $ unfoldr unfolder (elems $ screen lcd)
        where
            unfolder [] = Nothing
            unfolder lst = Just (take cols lst, drop cols lst)

main = do
    ls <- lines <$> getContents
    let finalLcd = foldl doOp startLcd (map parseOp ls)
    print $ finalLcd
    print $ length $ filter (== '#') $ elems $ screen finalLcd