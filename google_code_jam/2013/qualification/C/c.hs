import Control.Applicative

-- |
-- >>> mapCase ["hoge","piyo"]
-- ["Case #1: hoge","Case #2: piyo"]
mapCase :: [String] -> [String]
mapCase xs = 
    let cases = map (\x->"Case #" ++ show x ++ ": ") [1..]
    in
      getZipList $ (++) <$> ZipList cases <*> ZipList xs

main = do
  contents <- getContents
  let ls = drop 1 $ lines contents
      q = map parse ls
      result = map (\(a,b) -> countFairSquare a b) q
  mapM putStrLn (mapCase (map show result))

-- |
-- >>> parse "9999999999997 11111111111119"
-- (9999999999997,11111111111119)
parse :: String -> (Integer, Integer)
parse xs =
    let
        ps = words xs
        a = (read $ ps!!0)
        b = (read $ ps!!1)
    in (a,b)
-- 
-- >>> countFairSquare 1 4
-- 2
-- >>> countFairSquare 10 120
-- 0
-- >>> countFairSquare 100 1000
-- 2
countFairSquare :: Integer -> Integer -> Int
countFairSquare a b =
    let r = [a..b]
        fss = filter (\x -> (checkFair x) && (checkSquareFair x)) r
    in length fss

-- |
-- >>> checkFair 6
-- True
-- >>> checkFair 11
-- True
-- >>> checkFair 121
-- True
-- >>> checkFair 10
-- False
-- >>> checkFair 12
-- False
-- >>> checkFair 123
-- False
checkFair :: Integer -> Bool
checkFair n =
    let str = show n
        len = length str
    in
      all (\x -> (str!!x) == (str!!(len-1-x))) [0..(div len 2)]

-- |
-- >>> checkSquareFair 16
-- True
-- >>> checkSquareFair 101
-- False
-- >>> checkSquareFair 99999999980000000001
-- True
-- >>> checkSquareFair 676
-- False
checkSquareFair :: Integer -> Bool
checkSquareFair n =
    let s = sqrt $ fromIntegral n
        fs = truncate s
    in 
      checkFair fs && fs*fs == n

