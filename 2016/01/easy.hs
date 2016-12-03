import Common
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

doOneMove :: Point -> Vector -> Int -> Point
doOneMove (x, y) (dx, dy) steps = (doOneMove' x dx, doOneMove' y dy)
    where
        doOneMove' start delta = start + (delta * steps)

doAllMoves :: [Instruction] -> Point
doAllMoves = fst . (foldl doInstruction ((0, 0), directions))
    where
        doInstruction (p, vz) (Instruction rot sc) =
            let rotated = rotate rot vz in
            (doOneMove p (head $ fst rotated) sc, rotated)

solve :: T.Text -> Int
solve = taxicab . doAllMoves . parseInstructions
    where
        taxicab (x, y) = (abs x) + (abs y)

main = ((show . solve) <$> T.getLine) >>= putStrLn
