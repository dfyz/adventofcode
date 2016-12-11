import Test.Hspec
import Text.Trifecta

data Marker = Marker {
    len :: Integer,
    count :: Integer
}

data DecodingMode = Easy | Hard

markerLength (Marker len count) = (length "(x)") + (length $ show len) + (length $ show count)

parseMarker = between (char '(') (char ')') parseMarker'
    where
        parseMarker' = do
            l <- decimal
            char 'x'
            c <- decimal
            return $ Marker l c

decode :: DecodingMode -> String -> Integer
decode mode str = decode' str
    where
        decode' [] = 0
        decode' str@(x:xs) =
            case (parseString parseMarker mempty str) of
                (Success m@(Marker len count)) ->
                    let
                        rest = drop (fromIntegral $ (markerLength m)) str
                        (before, after) = splitAt (fromIntegral len) rest
                        beforeLen = case mode of
                            Easy -> len
                            Hard -> decode' before
                    in
                        (beforeLen * count) + (decode' after)
                _ -> 1 + (decode' xs)

main = do
    hspec $ do
        describe "decode easy" $ do
            it "sample #1" $ do
                decode Easy "ADVENT" `shouldBe` (toInteger $ length "ADVENT")
            it "sample #2" $ do
                decode Easy "A(1x5)BC" `shouldBe` (toInteger $ length "ABBBBBC")
            it "sample #3" $ do
                decode Easy "(3x3)XYZ" `shouldBe` (toInteger $ length "XYZXYZXYZ")
            it "sample #4" $ do
                decode Easy "A(2x2)BCD(2x2)EFG" `shouldBe` (toInteger $ length "ABCBCDEFEFG")
            it "sample #5" $ do
                decode Easy "(6x1)(1x3)A" `shouldBe` (toInteger $ length "(1x3)A")
            it "sample #6" $ do
                decode Easy "X(8x2)(3x3)ABCY" `shouldBe` (toInteger $ length "X(3x3)ABC(3x3)ABCY")

        describe "decode hard" $ do
            it "sample #1" $ do
                decode Hard "(3x3)XYZ" `shouldBe` (toInteger $ length "XYZXYZXYZ")
            it "sample #2" $ do
                decode Hard "X(8x2)(3x3)ABCY" `shouldBe` (toInteger $ length "XABCABCABCABCABCABCY")
            it "sample #3" $ do
                decode Hard "(27x12)(20x12)(13x14)(7x10)(1x12)A" `shouldBe` 241920
            it "sample #4" $ do
                decode Hard "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" `shouldBe` 445

    contents <- init <$> getContents
    print $ (decode Easy contents)
    print $ (decode Hard contents)
