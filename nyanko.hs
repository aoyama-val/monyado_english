import System.IO
import System.Exit
import System.Random

-- TODO: end of lineの処理

{-| リストの中からランダムに要素を選んで返す -}
sample :: [a] -> IO a
sample [] = error "sample: empty list"
sample xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

        
{-| リストxsの中からn個のインデックスをランダムに選んで返す-}
sampleN n xs result
    | n == 0                = do return result
    | length xs < n         = error "sampleN: length xs < n"
    | otherwise             = do
        i <- randomRIO (0, (length xs) - 1)
        if i `elem` result
            then sampleN n xs result
            else sampleN (n - 1) xs (i : result)


{-| 文字列sのi番目の文字を*で置き換える

>>> maskChar "hoge" 0
"*oge"

-}
maskChar :: String -> Int -> String
maskChar s i = (take i s) ++ "*" ++ (drop (i + 1) s)


{-| 引数で与えられた位置の文字をマスクする

>>> maskWord "lambda" [2, 4]
"la*b*a"

 -}
maskWord :: String -> [Int] -> String
maskWord s [] = s
maskWord s (x:xs) = maskWord (maskChar s x) xs


{-| 単語がマスクされた単語にマッチするか判定する

>>> isMatch "h*g*" "hoge"
True

>>> isMatch "h*g*" "hag"
False

>>> isMatch "h*g*" "hogera"
False

-}
isMatch :: String -> String -> Bool
isMatch "" "" = True
isMatch "" _  = False
isMatch _  "" = False
isMatch masked@(x:xs) answer@(y:ys) = (x == '*' || x == y) && isMatch xs ys

{-| 単語の長さに応じてマスクする文字の個数を返す -}
maskCount :: String -> Int
maskCount word
    | len <= 5  = 1
    | len <= 8  = 2
    | otherwise = 3
    where len = length word

{-| 与えられた単語をランダムにマスクする -}
maskWordRandomly :: String -> IO String
maskWordRandomly word = do
    positions <- sampleN (maskCount word) word []
    return $ maskWord word positions

{-| ランダムに問題を作る -}
makeProblem :: [String] -> IO String
makeProblem words = do
    word <- sample words
    maskWordRandomly word

{-| ファイルから単語リストを読み込む -}
loadWords :: String -> IO [String]
loadWords filename = do
    content <- readFile filename
    return $ lines content

{-| ニャンコに発言させる -}
nyankoSays :: String -> IO ()
nyankoSays str = do 
    putStrLn $ "ニャンコ: " ++ str

{-| 永遠に問題を出し続ける-}
problemLoop words = do
    problem <- makeProblem words
    nyankoSays $ "「" ++ problem ++ "」に当てはまる英単語を答えるニャ"
    acceptAnswerLoop problem words
    problemLoop words

{-| ユーザー入力を待って正解するまで繰り返す-}
acceptAnswerLoop problem words = do
    putStr "> " >> hFlush stdout
    done <- isEOF
    if done
        then do
            exitSuccess
        else do
            answer <- getLine
            if answer == "正解は？" || answer == "?"
                then do
                    nyankoSays "しょうがにゃいニャ～"
                    nyankoSays $ show $ filter (\x -> isMatch problem x) words
            else if isMatch problem answer && answer `elem` words
                    then do
                        nyankoSays "正解だニャ"
                    else do
                        nyankoSays "不正解だニャ"
                        acceptAnswerLoop problem words

main :: IO ()
main = do
    --let word = "lambda"
    --print $ map (maskChar word) [0..5]
    --positions <- sampleN 3 word []
    --print $ positions
    --print =<< sampleN 3 [1..2] []
    --print $ maskWord word positions
    --print =<< maskWordRandomly word

    words <- loadWords "words.txt"
    --print $ take 10 words
    --print =<< sample "hoge"
    --print =<< sample "hoge"
    --print =<< sample "hoge"

    problemLoop words
