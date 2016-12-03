module Common
where

import qualified Data.Text as T

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

parseInstructions :: T.Text -> [Instruction]
parseInstructions = (map parseOneIns) . (T.splitOn (T.pack ", "))
    where
        parseOneIns t = Instruction (T.head t) (read $ T.unpack $ T.tail t)

