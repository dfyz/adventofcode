{-# LANGUAGE TupleSections #-}

import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple
import Text.ParserCombinators.ReadP

data Room = Room {
    roomWords :: [String],
    roomCode :: Int,
    roomChecksum :: String
} deriving Show

parseRoom :: String -> Room
parseRoom = fst . head . (readP_to_S parseRoom')
    where
        parseRoom' = do
            tokens <- sepBy (many $ satisfy isAsciiLower) (char '-')
            char '-'
            code <- many (satisfy isDigit)
            checksum <- between (char '[') (char ']') (many $ satisfy isAsciiLower)
            eof
            return $ Room tokens (read code) checksum

isValidRoom :: Room -> Bool
isValidRoom (Room words _ checksum) = checksum == (getChecksum $ concat words)
    where
        getChecksum = (take 5) . (fmap fst) . (sortOn (swap . fmap negate)) .  M.toList . (M.fromListWith (+)) . (fmap (,1))

roomToPhrase :: Room -> (String, Int)
roomToPhrase (Room words code _) = (intercalate " " $ map (map rotCh) words, code)
    where
        alphabet = ['a'..'z']
        rotCh ch = alphabet !! (((fromJust $ elemIndex ch alphabet) + code) `mod` (length alphabet))

main = do
    rooms <- (map parseRoom) <$> (lines <$> getContents)
    let easyAnswer = sum $ (map roomCode) $ (filter isValidRoom rooms)
    let hardAnswer = snd $ head $ filter (isInfixOf "north" . fst) $ map roomToPhrase rooms
    putStrLn $ show (easyAnswer, hardAnswer)
