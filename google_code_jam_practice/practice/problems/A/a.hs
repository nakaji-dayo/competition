import CodeJamUtil
import Data.List

main = do
    contents <- getContents
    let
        ls = lines contents
        cases = tail ls
        results = map solve cases
    mapM putStrLn (mapCase results)

solve :: String -> String
solve xs = 
    let
        (num, src, dst) = getParams3 xs
        ten = toTenDigit src num
        alian = fromTenDigit dst ten
    in
      alian

-- |
-- >>> toTenDigit "0123456789" "9" 
-- 9
-- >>> toTenDigit "oF8" "Foo"
-- 9
toTenDigit :: String -> String ->  Int
toTenDigit src num = 
    let
        n = length src
        len = length num 
    in
      foldl (\acc x -> acc + (getDigitValue src (num!!(len-x-1))) * (n^x)) 0 [0..(length num)-1]

-- |
-- >>> fromTenDigit "oF8" 9
-- "Foo"
fromTenDigit :: String -> Int -> String
fromTenDigit dst num =
    let
        max = maxDigit dst num
        n = length dst
        r = foldl (\(str,m) x -> func1 dst m n x str) ([],num) [max,max-1..0]
    in
      fst r

-- |
-- >>> func1 "oF8" 9 3 2 []
-- ("F",0)
func1 :: String -> Int -> Int -> Int -> [Char] -> ([Char], Int)
func1 dst m n x str =
    let nv = (n^x)
        v = (dst!!) $ truncate ((fromIntegral m)/(fromIntegral nv))
    in
       (str++[v], m `mod` nv)

-- |
-- >>> maxDigit "0123456789" 13
-- 1
-- >>> maxDigit "01" 19
-- 4
-- >>> maxDigit "oF8" 9
-- 2
maxDigit :: String -> Int -> Int
maxDigit dst num =
    let
        n = length dst
    in
      truncate $ logBase (fromIntegral n) (fromIntegral num)

-- |
-- >>> getDigitValue "oF8" 'F'
-- 1
-- >>> getDigitValue "0123456789" '9'
-- 9
getDigitValue :: String -> Char -> Int
getDigitValue src c = 
    let (Just i) = c `elemIndex` src
    in i
