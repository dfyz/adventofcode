import Control.Applicative
import Data.List
import Data.Maybe
import Data.Tuple
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

insides = mapMaybe inside
    where
        inside (InsideBrackets x) = Just x
        inside _ = Nothing

outsides = mapMaybe outside
    where
        outside (OutsideBrackets x) = Just x
        outside _ = Nothing

hasAbba :: String -> Bool
hasAbba lst@(a:b:c:d:_) = isGoodQuadruplet || (hasAbba $ tail lst)
    where
        isGoodQuadruplet = (a == d) && (b == c) && (a /= b)
hasAbba _ = False

hasTls :: IPv7 -> Bool
hasTls (IPv7 parts) = (any hasAbba (outsides parts)) && (all (not . hasAbba) (insides parts))

collectAbas :: String -> [(Char, Char)]
collectAbas = mapMaybe extractAba . tails
    where
        extractAba (a:b:c:_)
            | (a == c) && (a /= b) = Just (a, b)
            | otherwise = Nothing
        extractAba _ = Nothing

hasSsl :: IPv7 -> Bool
hasSsl (IPv7 parts) = not $ null $ intersect (concatMap collectAbas (outsides parts)) (swap <$> concatMap collectAbas (insides parts)) 

main = do
    hspec $ do
        describe "hasTls" $ do
            it "sample #1" $ do
                "abba[mnop]qrst" `shouldSatisfy` parseAndCheckTls
            it "sample #2" $ do
                "abcd[bddb]xyyx" `shouldNotSatisfy` parseAndCheckTls
            it "sample #3" $ do
                "aaaa[qwer]tyui" `shouldNotSatisfy` parseAndCheckTls
            it "sample #4" $ do
                "ioxxoj[asdfgh]zxcvbn" `shouldSatisfy` parseAndCheckTls

        describe "hasSsl" $ do
            it "sample #1" $ do
                "aba[bab]xyz" `shouldSatisfy` parseAndCheckSsl
            it "sample #2" $ do
                "xyx[xyx]xyx" `shouldNotSatisfy` parseAndCheckSsl
            it "sample #3" $ do
                "aaa[kek]eke" `shouldSatisfy` parseAndCheckSsl
            it "sample #4" $ do
                "zazbz[bzb]cdb" `shouldSatisfy` parseAndCheckSsl


    ls <- lines <$> getContents
    print $ count parseAndCheckTls ls
    print $ count parseAndCheckSsl ls

    where
        parseAndCheckTls = hasTls . parseIPv7
        parseAndCheckSsl = hasSsl . parseIPv7
        count pred = length . filter pred
