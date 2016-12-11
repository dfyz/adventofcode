import Control.Applicative
import Data.Maybe
import Test.Hspec
import Text.Trifecta

data IPv7Part
    = InsideBrackets String
    | OutsideBrackets String
    deriving (Show)

newtype IPv7 = IPv7 [IPv7Part] deriving (Show)

parseIPv7 :: String -> IPv7
parseIPv7 = fromSuccess . (parseString (IPv7 <$> some tk) mempty)
    where
        fromSuccess (Success x) = x
        letters = some lower
        tk = (OutsideBrackets <$> letters) <|> (InsideBrackets <$> between (char '[') (char ']') letters)

hasAbba :: String -> Bool
hasAbba lst@(a:b:c:d:_) = isGoodQuadruplet || (hasAbba $ tail lst)
    where
        isGoodQuadruplet = (a == d) && (b == c) && (a /= b)
hasAbba _ = False

hasTls :: IPv7 -> Bool
hasTls (IPv7 parts) = (any hasAbba (mapMaybe outside parts)) && (all (not . hasAbba) (mapMaybe inside parts))
    where
        inside (InsideBrackets x) = Just x
        inside _ = Nothing
        outside (OutsideBrackets x) = Just x
        outside _ = Nothing

main = do
    hspec $ do
        describe "hasTls" $ do
            it "sample #1" $ do
                "abba[mnop]qrst" `shouldSatisfy` parseAndCheck
            it "sample #2" $ do
                "abcd[bddb]xyyx" `shouldNotSatisfy` parseAndCheck
            it "sample #3" $ do
                "aaaa[qwer]tyui" `shouldNotSatisfy` parseAndCheck
            it "sample #4" $ do
                "ioxxoj[asdfgh]zxcvbn" `shouldSatisfy` parseAndCheck

    solveEasy >>= print

    where
        parseAndCheck = hasTls . parseIPv7
        solveEasy = (length . filter parseAndCheck) <$> lines <$> getContents