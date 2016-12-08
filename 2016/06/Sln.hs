{-# LANGUAGE TupleSections #-}

import Data.List
import qualified Data.Map as M
import Data.Tuple

byFreq :: String -> String
byFreq = fmap fst . (sortOn (swap . fmap negate)) .  M.toList . (M.fromListWith (+)) . (fmap (,1))

solve getter = fmap getter . fmap byFreq . transpose

solveEasy = solve head
solveHard = solve last  

main = do
    ls <- lines <$> getContents
    putStrLn $ solveEasy ls
    putStrLn $ solveHard ls