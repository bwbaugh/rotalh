{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.Printf (printf)
import Data.Version (showVersion)

import Development.GitRev
import Options.Applicative
import Paths_rotalh (version)
import System.Console.ANSI (clearFromCursorToLineEnd, cursorUp)
import qualified Data.HashMap.Strict as HM

data Options = Options
    { optPercent :: !Bool
    } deriving (Show)

options :: Parser (Maybe Options)
options = flag' Nothing (long "version" <> help "Show version.")
    <|> (Just <$> normal_options)
  where
    normal_options = Options
        <$> switch
            ( long "percent"
            <> short 'p'
            <> help "Show percent of total for each line."
            )

main :: IO ()
main = execParser opts >>= maybe (putStrLn versionString) run
  where
    opts = info (helper <*> options)
        ( fullDesc
        <> progDesc "Intended to be a replacement for `sort | uniq -c`." )
    versionString = concat
        [ "v", showVersion version
        , " ("
        , $(gitBranch), "@", $(gitHash)
        , "; ", $(gitCommitDate)
        , "; ", $(gitCommitCount), " commits in HEAD"
        , ")"
        ]

run :: Options -> IO ()
run opts = do
    allLines <- fmap lines getContents
    seenMVar <- newMVar HM.empty
    numPrevLinesMVar <- newMVar 0
    -- XXX: Maybe there's a better way than calling this with undefined.
    let outputFunc _ = displayOutput numPrevLinesMVar seenMVar opts
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

displayOutput :: MVar Int
              -> MVar (HM.HashMap String Integer)
              -> Options
              -> IO ()
displayOutput numPrevLinesMVar seenMVar opts = do
    seen <- readMVar seenMVar
    numPrevLines <- takeMVar numPrevLinesMVar
    when (numPrevLines > 0) $ cursorUp numPrevLines
    let status = makeStatus seen opts
    showStatus status
    -- XXX: Using the MVar as a lock on updating the screen.
    putMVar numPrevLinesMVar (length status)

showStatus :: [String] -> IO ()
showStatus status = forM_ status $ \line -> do
    clearFromCursorToLineEnd
    putStrLn line

makeStatus :: HM.HashMap String Integer -> Options -> [String]
makeStatus seen opts = map unwords sortedValues
  where
    sortedValues = sortBy (comparing last) countValuePairs
    countValuePairs = map formatPair (HM.toList seen)
    formatPair (word, count) = catMaybes
        [ Just $ printf "%4d" count
        , if optPercent opts then Just (percent count) else Nothing
        , Just word
        ]
    percent count = printf "(%.2f%%)" ((fromIntegral count :: Float) / total * 100)
    total = fromIntegral $ sum (HM.elems seen)
