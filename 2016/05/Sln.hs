{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Array.ST
import Data.Array.IArray
import Data.Char
import Data.Digest.Pure.MD5
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

doorId = "ffykfhsq"
zeroCount = 5
passLen = 8

md5Inputs = ((doorId ++) . show) <$> [1..]
toMd5s = fmap $ show . md5 . BC.pack
prefix = take zeroCount $ repeat '0'
goodMd5sOnly = (filter (isPrefixOf prefix))
inputStream = goodMd5sOnly $ toMd5s $ md5Inputs

solveEasy :: String
solveEasy = concat (md5ToSymbol <$> (take passLen $ inputStream))
    where        
        md5ToSymbol = take 1 . drop zeroCount

solveHard :: String
solveHard = map fromJust $ elems $ runSTArray $ do
    password <- newArray (0, 7) Nothing
    count <- newSTRef (0 :: Int)
    maxPos <- snd <$> getBounds password
    fillPassword password count maxPos inputStream
    return password
        where
            fillPassword password countRef maxPos (m:ms) = do
                count <- readSTRef countRef
                if (count >= passLen) then return password
                else do
                    let (pos:val:_) = take 2 $ drop zeroCount m
                    let intPos = digitToInt pos
                    let res = fillPassword password countRef maxPos ms
                    if (intPos > maxPos) then res
                    else do
                        prevVal <- readArray password intPos
                        when (isNothing prevVal) $ do
                            modifySTRef countRef (+1)
                            writeArray password intPos (Just val)
                        res

main = do
    putStrLn $ solveEasy
    putStrLn $ solveHard