import Data.List (elemIndex, intersperse)
import Data.Maybe (fromJust)
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
    status = unlines $ map concat $ map (intersperse "\t") [[word, show count] | (word, count) <- zip seen' counts']
    seen'
        | x `elem` seen = seen
        | otherwise = x : seen
    counts'
        | x `elem` seen = updateIndex (fromJust $ elemIndex x seen) counts (counts !! (fromJust $ elemIndex x seen) + 1)
        | otherwise = 1 : counts

updateIndex :: Int -> [a] -> a -> [a]
updateIndex k xs newx = take k xs ++ [newx] ++ drop (k + 1) xs
