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
    numPrevLinesMVar <- newMVar (-1)
    -- XXX: Maybe there's a better way than calling this with undefined.
    let outputFunc _ = displayOutput numPrevLinesMVar seenMVar showPercent
    workerId <- forkIO (outputWorker outputFunc)
    forM_ allLines $ \x -> do
        seen <- takeMVar seenMVar
        putMVar seenMVar (HM.insertWith (+) x 1 seen)
    -- Acquire a lock for updating the screen for the final output.
    numPrevLines <- takeMVar numPrevLinesMVar
    killThread workerId
    putMVar numPrevLinesMVar numPrevLines
    outputFunc undefined

outputWorker :: (a -> IO b) -> IO ()
outputWorker f = forever $ f undefined >> threadDelay 1000000

displayOutput :: MVar Int -> MVar (HM.HashMap String Integer) -> Bool -> IO ()
displayOutput numPrevLinesMVar seenMVar showPercent = do
    seen <- readMVar seenMVar
    numPrevLines <- takeMVar numPrevLinesMVar
    cursorUpLine numPrevLines
    if null seen
        -- XXX: Need to store -1 to prevent moving cursor up too far.
        -- Without this, the cursor would be moved up to before the
        -- program started.
        then putMVar numPrevLinesMVar (-1)
        else do
            let status = makeStatus seen showPercent
            showStatus status
            -- XXX: Using the MVar as a lock on updating the screen.
            putMVar numPrevLinesMVar (length status)

showStatus :: [String] -> IO ()
showStatus status = forM_ status $ \line -> do
    clearFromCursorToLineEnd
    putStrLn line

makeStatus :: HM.HashMap String Integer -> Bool -> [String]
makeStatus seen showPercent = map unwords sortedValues
  where
    sortedValues = sortBy (comparing last) countValuePairs
    -- TODO(bwbaugh|2015-11-13): Pull out all formatting to another function.
    countValuePairs
        | showPercent = [[printf "%4d" count, percent count, word] | (word, count) <- HM.toList seen]
        | otherwise = [[printf "%4d" count, word] | (word, count) <- HM.toList seen]
      where
        percent count = printf "(%.2f%%)" ((fromIntegral count :: Float) / total * 100)
        total = fromIntegral $ sum (HM.elems seen)
