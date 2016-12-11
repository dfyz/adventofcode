import Test.Hspec
import Text.Trifecta

data Marker = Marker {
    len :: Int,
    count :: Int
}

markerLength (Marker len count) = (length "(x)") + (length $ show len) + (length $ show count)

decode :: String -> String
decode [] = []
decode str@(x:xs) =
    case (parseString parseMarker mempty str) of
        (Success m@(Marker len count)) ->
            let
                (_, rest) = splitAt (markerLength m) str
                (before, after) = splitAt len rest
            in
                (concat $ replicate count before) ++ (decode after)
        _ -> x:(decode xs)
        where
            parseMarker = between (char '(') (char ')') parseMarker'
            parseMarker' = do
                l <- decimal
                char 'x'
                c <- decimal
                return $ Marker (fromIntegral l) (fromIntegral c)

main = do
    hspec $ do
        describe "decode" $ do
            it "sample #1" $ do
                decode "ADVENT" `shouldBe` "ADVENT"
            it "sample #2" $ do
                decode "A(1x5)BC" `shouldBe` "ABBBBBC"
            it "sample #3" $ do
                decode "(3x3)XYZ" `shouldBe` "XYZXYZXYZ"
            it "sample #4" $ do
                decode "A(2x2)BCD(2x2)EFG" `shouldBe` "ABCBCDEFEFG"
            it "sample #5" $ do
                decode "(6x1)(1x3)A" `shouldBe` "(1x3)A"
            it "sample #6" $ do
                decode "X(8x2)(3x3)ABCY" `shouldBe` "X(3x3)ABC(3x3)ABCY"

    (length . init . decode) <$> getContents >>= print