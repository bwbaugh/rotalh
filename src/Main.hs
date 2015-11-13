import Data.List (elemIndex)
import System.Console.ANSI (clearFromCursorToLineEnd, cursorUpLine)

main :: IO ()
main = do
    contents <- getContents
    let allLines = lines contents
    loop (-1) $ runTotal [] [] allLines

loop :: Int -> [String] -> IO ()
loop _ [] = return ()
loop numPrevLines (status:xs) = do
    cursorUpLine $ numPrevLines
    showStatus status
    loop (length $ lines status) xs

showStatus :: String -> IO ()
showStatus status = do
    mapM_ showCurrentStatus $ lines status
        where
        showCurrentStatus line = do
            clearFromCursorToLineEnd
            putStrLn line

runTotal :: [String] -> [Integer] -> [String] -> [String]
runTotal _ _ [] = []
runTotal seen counts (x:xs) = status : (runTotal seen' counts' xs)
    where
    status = unlines $ map unwords [[padR 4 (show count), word] | (count, word) <- zip counts' seen']
    xIndexMaybe = elemIndex x seen
    seen' = case xIndexMaybe of
        Just _ -> seen
        Nothing -> x : seen
    counts' = case xIndexMaybe of
        Just xIndex -> updateIndex xIndex counts (counts !! xIndex + 1)
        Nothing -> 1 : counts

updateIndex :: Int -> [a] -> a -> [a]
updateIndex k xs newx = take k xs ++ [newx] ++ drop (k + 1) xs

padR :: Int -> String -> String
padR n s
    | length s < n  = replicate (n - length s) ' ' ++ s
    | otherwise = s
