module Main where

import Control.Monad
import Data.List (elemIndex, sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)

import System.Console.ANSI (clearFromCursorToLineEnd, cursorUpLine)

main :: IO ()
main = do
    args <- getArgs
    let showPercent = any (`elem` args) ["-p", "--percent"]
    allLines <- fmap lines getContents
    loop (-1) $ runTotal [] [] showPercent allLines

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

runTotal :: [String] -> [Integer] -> Bool -> [String] -> [String]
runTotal _ _ _ [] = []
runTotal seen counts showPercent (x:xs) = status : runTotal seen' counts' showPercent xs
    where
    status = unlines $ map unwords sortedValues
    sortedValues = sortBy (comparing last) countValuePairs
    -- TODO(bwbaugh|2015-11-13): Pull out all formatting to another function.
    countValuePairs
        | showPercent = [[printf "%4d" count, percent count, word] | (count, word) <- zip counts' seen']
        | otherwise = [[printf "%4d" count, word] | (count, word) <- zip counts' seen']
        where
        percent count = printf "(%.2f%%)" ((fromIntegral count :: Float) / total * 100)
        total = fromIntegral $ sum counts'
    xIndexMaybe = elemIndex x seen
    seen' = case xIndexMaybe of
        Just _ -> seen
        Nothing -> x : seen
    counts' = case xIndexMaybe of
        Just xIndex -> updateIndex xIndex counts (counts !! xIndex + 1)
        Nothing -> 1 : counts

updateIndex :: Int -> [a] -> a -> [a]
updateIndex k xs newx = take k xs ++ [newx] ++ drop (k + 1) xs
