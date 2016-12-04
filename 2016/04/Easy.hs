{-# LANGUAGE TupleSections #-}

import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Tuple
import Text.ParserCombinators.ReadP

data Room = Room {
    roomSyms :: String,
    roomCode :: Int,
    roomChecksum :: String
} deriving Show

parseRoom :: String -> Room
parseRoom = fst . head . (readP_to_S parseRoom')
    where
        parseRoom' = do
            tokens <- sepBy (many $ satisfy isAsciiLower) (char '-')
            code <- many (satisfy isDigit)
            checksum <- between (char '[') (char ']') (many $ satisfy isAsciiLower)
            eof
            return $ Room (concat tokens) (read code) checksum

isValidRoom :: Room -> Bool
isValidRoom (Room syms _ checksum) = checksum == (getChecksum syms)
    where
        getChecksum = (take 5) . (fmap fst) . (sortOn (swap . fmap negate)) .  M.toList . (M.fromListWith (+)) . (fmap (,1))

main = do
    answer <- sum <$> (map roomCode) <$> (filter isValidRoom) <$> (map parseRoom) <$> (lines <$> getContents)
    putStrLn $ show answer
