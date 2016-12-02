import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Vector = (Int, Int)
type Point = Vector
type VectorZipper = ([Vector], [Vector])

data Instruction = Instruction {
    rotation :: Char,
    stepCount :: Int
}

directions :: VectorZipper
directions = (dirs, [])
    where
        dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

goLeft :: VectorZipper -> VectorZipper
goLeft (xs, []) = ([last xs], reverse $ init xs)
goLeft (xs, y:ys) = (y:xs, ys)

goRight :: VectorZipper -> VectorZipper
goRight (x:[], ys) = ((reverse ys) ++ [x], [])
goRight (x:xs, ys) = (xs, x:ys)

doOneMove :: Point -> VectorZipper -> Int -> Point
doOneMove (x, y) ((dx, dy):_, _) steps = (doOneMove' x dx, doOneMove' y dy)
    where
        doOneMove' start delta = start + (delta * steps)

doAllMoves :: [Instruction] -> Point
doAllMoves = fst . (foldl doInstruction ((0, 0), directions))
    where
        doInstruction (p, vz) (Instruction rot sc) =
            let rotated = rotate rot vz in
            (doOneMove p rotated sc, rotated)
        rotate 'L' = goLeft
        rotate 'R' = goRight

parseInstructions :: T.Text -> [Instruction]
parseInstructions = (map parseOneIns) . (T.splitOn (T.pack ", "))
    where
        parseOneIns t = Instruction (T.head t) (read $ T.unpack $ T.tail t)

solve :: T.Text -> Int
solve = taxicab . doAllMoves . parseInstructions
    where
        taxicab (x, y) = (abs x) + (abs y)

main = ((show . solve) <$> T.getLine) >>= putStrLn
