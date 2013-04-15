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
  let ls = lines contents
      con = drop 1 ls
      q = makeQuestions con
      result = map (\(n,m,xs)->check n m xs) q
  mapM putStrLn (mapCase (map objCwww result))

objCwww :: Bool -> String
objCwww True = "YES"
objCwww False = "NO"

-- |
-- >>> makeQuestions ["3 3","2 1 2","1 1 1","2 1 2","1 3","1 2 1"]
-- [(3,3,[2,1,2,1,1,1,2,1,2]),(1,3,[1,2,1])]
makeQuestions :: [String] -> [(Int,Int,[Int])]
makeQuestions [] = []
makeQuestions xs = 
    let
        (n,m,r) = getHead xs
        con = take n r
        ls = map words con
        square = concat ls
        nums = map read square
        after = drop n r
    in
      (n, m, nums): makeQuestions after

-- |
-- >>> getHead ["3 3", "2 1 2", "1 1 1", "2 1 2", "1 3", "1 2 1"]
-- (3,3,["2 1 2","1 1 1","2 1 2","1 3","1 2 1"])
getHead :: [String] -> (Int,Int,[String])
getHead xs = 
    let
        params = words (xs!!0)
        r = drop 1 xs
        n = (read $ params !! 0)::Int
        m = (read $ params !! 1)::Int
    in
      (n, m, r)

-- |
-- >>> check 3 3 [2,1,2,1,1,1,2,1,2]
-- True
-- >>> check 5 5 [2,2,2,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1,1,2,2,2,2,2,2]
-- False
-- >>> check 1 3 [1,2,1] 
-- True
check :: Int -> Int -> [Int] -> Bool
check n m xs = 
    let r = checkRec n m xs (Just 0)
    in
      if r == Just(m*n)
      then True
      else False

checkRec :: Int -> Int -> [Int] -> Maybe Int -> Maybe Int
checkRec _ _ _ Nothing = Nothing
checkRec n m xs index  
    | index >= Just(n*m) = Just(m*n)
    | otherwise =                 
        let r = checkCell n m xs index
        in
          checkRec n m xs r

-- | 
-- セルの十字方向にそれより高い芝がないルートが有るかチェック
-- あった場合nothing、なければ次にチェックすべきindexを返す
-- >>> checkCell 3 3 [2,1,2,1,1,1,2,1,2] (Just 0)
-- Just 1
-- >>> checkCell 3 3 [2,1,2,1,1,1,2,1,2] (Just 3)
-- Just 4
-- >>> checkCell 3 3 [2,1,2,1,1,1,2,1,2] (Just 1)
-- Just 2
-- 
-- >>> checkCell 1 1 [9,9,9] Nothing
-- Nothing
-- 
-- >>> checkCell 5 5 [2,2,2,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1,1,2,2,2,2,2,2] (Just 0)
-- Just 1
-- >>> checkCell 5 5 [2,2,2,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1,1,2,2,2,2,2,2] (Just 6)
-- Nothing
-- 
-- >>> checkCell 1 3 [1,2,1] (Just 0)
-- Just 1
-- >>> checkCell 1 3 [1,2,1] (Just 1)
-- Just 2
checkCell :: Int -> Int -> [Int] -> Maybe Int -> Maybe Int
checkCell _ _ _ Nothing = Nothing
checkCell n m xs (Just index) = 
    let c = xs !! index
        left = getLeftIndex m index
        colIndex = [left..(left+m-1)]
        cols = map (xs !!) colIndex
        top = getTopIndex m index
        rowIndex = makeSkipList top m n
        rows = map (xs !!) rowIndex
    in
      if (any (>c) cols) && (any (>c) rows)
      then Nothing
      else Just (index+1)

makeSkipList :: Int -> Int -> Int -> [Int]
makeSkipList start skip num = 
    let a = [0..num-1]
        b = map (*skip) a
        c = map (+start) b
    in c
          
-- |
-- >>> getLeftIndex 3 1
-- 0
-- >>> getLeftIndex 3 3
-- 3
-- >>> getLeftIndex 5 13
-- 10
getLeftIndex :: Int -> Int -> Int
getLeftIndex m index = 
    (div index m) * m

-- |
-- >>> getTopIndex 4 1
-- 1
-- >>> getTopIndex 4 5
-- 1
-- >>> getTopIndex 3 3
-- 0
getTopIndex :: Int -> Int -> Int
getTopIndex m index = 
    mod index m
