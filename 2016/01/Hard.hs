import Common

import Control.Monad.State
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

doInstructions :: [Instruction] -> State (WalkerState, [Point]) [Point]

doInstructions [] = do
    (_, points) <- get
    return points

doInstructions ((Instruction rot sc):xs) = do
    ((start, vz), ps) <- get
    let rotated = rotate rot vz
    let v = currentDirection rotated
    let newPoints = take sc $ tail $ iterate (\x -> doOneMove x v 1) start
    put ((last newPoints, rotated), ps ++ newPoints)
    doInstructions xs

traceMoves :: [Instruction] -> [Point]
traceMoves instrs = evalState (doInstructions instrs) (initialState, [(0, 0)])

getRepeatingPoint :: [Point] -> Maybe Point
getRepeatingPoint = getRepeatingPoint' S.empty
    where
        getRepeatingPoint' visited points
            | null points = Nothing
            | S.member p visited = Just p
            | otherwise = getRepeatingPoint' (S.insert p visited) ps
            where
                p = head points
                ps = tail points

solve = taxicab. fromJust . getRepeatingPoint . traceMoves . parseInstructions
main = ((show . solve) <$> T.getLine) >>= putStrLn
