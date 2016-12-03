import Common

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

doAllMoves :: [Instruction] -> Point
doAllMoves = fst . (foldl doInstruction initialState)
    where
        doInstruction (p, vz) (Instruction rot sc) =
            let rotated = rotate rot vz in
            (doOneMove p (currentDirection rotated) sc, rotated)

solve = taxicab . doAllMoves . parseInstructions
main = ((show . solve) <$> T.getLine) >>= putStrLn
