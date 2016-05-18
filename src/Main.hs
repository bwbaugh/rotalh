module Main where

import Control.Concurrent
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
    seenMVar <- newMVar HM.empty
    _ <- forkIO (displayOutput (-1) seenMVar showPercent)
    forM_ allLines $ \x -> do
        seen <- takeMVar seenMVar
        putMVar seenMVar (HM.insertWith (+) x 1 seen)

displayOutput :: Int -> MVar (HM.HashMap String Integer) -> Bool -> IO ()
displayOutput numPrevLines seenMVar showPercent = forever $ do
    seen <- readMVar seenMVar
    cursorUpLine numPrevLines
    let status = makeStatus seen showPercent
    showStatus status
    threadDelay 1000000
    displayOutput (length $ lines status) seenMVar showPercent

showStatus :: String -> IO ()
showStatus status = forM_ (lines status) $ \line -> do
    clearFromCursorToLineEnd
    putStrLn line

makeStatus :: HM.HashMap String Integer -> Bool -> String
makeStatus seen showPercent = unlines $ map unwords sortedValues
  where
    sortedValues = sortBy (comparing last) countValuePairs
    -- TODO(bwbaugh|2015-11-13): Pull out all formatting to another function.
    countValuePairs
        | showPercent = [[printf "%4d" count, percent count, word] | (word, count) <- HM.toList seen]
        | otherwise = [[printf "%4d" count, word] | (word, count) <- HM.toList seen]
      where
        percent count = printf "(%.2f%%)" ((fromIntegral count :: Float) / total * 100)
        total = fromIntegral $ sum (HM.elems seen)
