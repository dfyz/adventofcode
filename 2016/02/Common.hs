module Common
where

import Control.Monad.State
import Data.Maybe

type Point = (Int, Int)
type ToDigitConverter = Point -> Maybe Char

charToPoint :: Char -> Point
charToPoint 'U' = (0, -1)
charToPoint 'D' = (0, 1)
charToPoint 'L' = (-1, 0)
charToPoint 'R' = (1, 0)

digitFromTable :: [String] -> Point -> Maybe Char
digitFromTable tbl (x, y)
    | not (inRange tbl y) = Nothing
    | not (inRange row x) = Nothing
    | d == ' ' = Nothing
    | otherwise = (Just d)
        where
            inRange lst idx = idx >= 0 && idx < (length lst)
            row = tbl !! y
            d = row !! x

moveBy :: ToDigitConverter -> Point -> Char -> Point
moveBy dc oldP@(x, y) ch = if isJust newDigit then newP else oldP
    where
        by = charToPoint ch
        moveCoord c d = c + d
        newP = (moveCoord x (fst by), moveCoord y (snd by))
        newDigit = dc newP

getCode' :: ToDigitConverter -> [String] -> State (Point, String) String

getCode' _ [] = do
    (_, code) <- get
    return code

getCode' td (x:xs) = do
    (p, code) <- get
    let newP = foldl (moveBy td) p x
    put (newP, code ++ [fromJust $ td newP])
    getCode' td xs

getCode :: Point -> ToDigitConverter -> [String] -> String
getCode start td ls = evalState (getCode' td ls) (start, "")
