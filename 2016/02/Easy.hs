import Control.Monad.State

type Point = (Int, Int)

dim :: Int
dim = 3

charToPoint :: Char -> Point
charToPoint 'U' = (0, -1)
charToPoint 'D' = (0, 1)
charToPoint 'L' = (-1, 0)
charToPoint 'R' = (1, 0)

pointToDigit :: Point -> Int
pointToDigit (x, y) = [1..9] !! (y * dim + x)

moveBy :: Point -> Char -> Point
moveBy (x, y) ch = (moveCoord x (fst by), moveCoord y (snd by))
    where
        by = charToPoint ch
        moveCoord c d = clamp (c + d) 0 (dim - 1)
        clamp val mn mx
            | val < mn = mn
            | val > mx = mx
            | otherwise = val

getCode :: [String] -> State (Point, String) String

getCode [] = do
    (_, code) <- get
    return code

getCode (x:xs) = do
    (p, code) <- get
    let newP = foldl moveBy p x
    put (newP, code ++ (show $ pointToDigit newP))
    getCode xs

main = do
    ls <- lines <$> getContents
    let code = evalState (getCode ls) ((0, 0), "")
    putStrLn code
