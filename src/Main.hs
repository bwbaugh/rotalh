module Main where

import Control.Monad
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)

import System.Console.ANSI (clearFromCursorToLineEnd, cursorUpLine)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    args <- getArgs
    let showPercent = any (`elem` args) ["-p", "--percent"]
    allLines <- fmap lines getContents
    loop (-1) $ runTotal HM.empty showPercent allLines

loop :: Int -> [String] -> IO ()
loop _ [] = return ()
loop numPrevLines (status:xs) = do
    cursorUpLine numPrevLines
    showStatus status
    loop (length $ lines status) xs

showStatus :: String -> IO ()
showStatus status = forM_ (lines status) $ \line -> do
    clearFromCursorToLineEnd
    putStrLn line

runTotal :: HM.HashMap String Integer -> Bool -> [String] -> [String]
runTotal _ _ [] = []
runTotal seen showPercent (x:xs) = status : runTotal seen' showPercent xs
    where
    status = unlines $ map unwords sortedValues
    sortedValues = sortBy (comparing last) countValuePairs
    -- TODO(bwbaugh|2015-11-13): Pull out all formatting to another function.
    countValuePairs
        | showPercent = [[printf "%4d" count, percent count, word] | (word, count) <- HM.toList seen']
        | otherwise = [[printf "%4d" count, word] | (word, count) <- HM.toList seen']
        where
        percent count = printf "(%.2f%%)" ((fromIntegral count :: Float) / total * 100)
        total = fromIntegral $ sum (HM.elems seen)
    seen' = HM.insertWith (+) x 1 seen
