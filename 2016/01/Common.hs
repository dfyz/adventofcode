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

type WalkerState = (Point, VectorZipper)

initialState :: WalkerState
initialState = ((0, 0), directions)

directions :: VectorZipper
directions = (dirs, [])
    where
        dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

currentDirection :: VectorZipper -> Vector
currentDirection vz = (head $ fst vz)

goLeft :: VectorZipper -> VectorZipper
goLeft (xs, []) = ([last xs], reverse $ init xs)
goLeft (xs, y:ys) = (y:xs, ys)

goRight :: VectorZipper -> VectorZipper
goRight (x:[], ys) = ((reverse ys) ++ [x], [])
goRight (x:xs, ys) = (xs, x:ys)

doOneMove :: Point -> Vector -> Int -> Point
doOneMove (x, y) (dx, dy) steps = (doOneMove' x dx, doOneMove' y dy)
    where
        doOneMove' start delta = start + (delta * steps)

parseInstructions :: T.Text -> [Instruction]
parseInstructions = (map parseOneIns) . (T.splitOn (T.pack ", "))
    where
        parseOneIns t = Instruction (T.head t) (read $ T.unpack $ T.tail t)

rotate :: Char -> VectorZipper -> VectorZipper
rotate 'L' = goLeft
rotate 'R' = goRight

taxicab :: Point -> Int
taxicab (x, y) = (abs x) + (abs y)
