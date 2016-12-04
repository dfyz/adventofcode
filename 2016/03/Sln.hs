import Control.Applicative
import Data.List

isTriangle :: [Int] -> Bool
isTriangle xs = all check (take (length xs) $ iterate rotateByOne xs)
    where
        check (a:b:c:_) = a + b > c
        rotateByOne (x:xs) = (xs ++ [x])

countTriangles :: [[Int]] -> Int
countTriangles = length . filter isTriangle

transformToHard :: [[Int]] -> [[Int]]
transformToHard = takeTriples . concat . transpose
    where
        takeTriples [] = []
        takeTriples (a:b:c:xs) = [a, b, c]:(takeTriples xs)

main = do
    ls <- lines <$> getContents
    let easyInput = map (map (read :: String -> Int) . words) ls
    let hardInput = transformToHard easyInput
    putStrLn $ show $ (countTriangles easyInput, countTriangles hardInput)
