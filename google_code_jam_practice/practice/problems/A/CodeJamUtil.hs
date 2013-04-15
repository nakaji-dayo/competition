module CodeJamUtil(
mapCase,
getParams3
) where 
import Control.Applicative

-- |
-- >>> mapCase ["hoge","piyo"]
-- ["Case #1: hoge","Case #2: piyo"]
mapCase :: [String] -> [String]
mapCase xs = 
    let cases = map (\x->"Case #" ++ show x ++ ": ") [1..]
    in
      getZipList $ (++) <$> ZipList cases <*> ZipList xs

getParams3 :: String -> (String, String, String)
getParams3 xs = 
    let 
        ws = words xs
    in
      (ws!!0, ws!!1, ws!!2)
