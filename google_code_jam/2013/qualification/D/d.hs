import Control.Applicative
import Data.List

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
      qs = (makeQuestions ls)
      results  = map solve qs
      out =  map (reverse . nub) results
      out2 = (map (map show) (out))
      out3 = map (filter (/="0")) out2 
      out9 = map emptoimp out3
      out4 = (map (unwords) out9)
  mapM putStrLn (mapCase out4)

emptoimp [] = ["IMPOSSIBLE"]
emptoimp xs = xs

solve cs =
    let
        graph = makeGraph cs
        result = allRouteWalk graph [] [] [] [] [] [] [] graph [] 0 ((length cs)-1)
    in result
             

makeQuestions :: [String] -> [[Chest]]
makeQuestions [] = []
makeQuestions xs =
    let (_,t,r) = getHead xs
        startk = map (read) $ words $ head r
        tres = take t $ drop 1 r
        tresN = map (\x -> (x, tres!!(x-1))) [1..t]
        chests = map (\(n,t) -> makeChest t n) tresN
        fc = (Chest 0 0 startk)
    in
      (fc:chests): (makeQuestions (drop (t+1) r))

makeChest :: String->Int -> Chest
makeChest xs n =
    let w = words xs
    in
      Chest n (read $ head w) (map read (drop 2 w))

getHead :: [String] -> (Int,Int,[String])
getHead xs = 
    let
        params = words (xs!!0)
        r = drop 1 xs
        n = (read $ params !! 0)::Int
        m = (read $ params !! 1)::Int
    in
      (n, m, r)


-- 宝箱　番号　鍵　中身
data Chest = Chest Int Int [Int]
             deriving(Show,Eq)

-- ノード　始点宝　[次にいけるノード]
data Node = Node Chest [Node] deriving (Show)

-- 
-- >>> walk 0 [] [0,1,-1,2,1,-1,4,3,-1]
-- [2,1,4,3]
walk :: Int -> [Int] ->[Int] -> [Int]
walk pos route xs
    | pos+2 == (length xs) = route
    | otherwise = 
    let
        v = xs!!(pos+1)
    in
      if (v == -1) then
          walk (pos+2) route xs
      else
          walk (pos+1) (v:route) xs
-- |
-- >>> allRoute (Node (Chest 0 0 [5]) [Node (Chest 2 5 [4]) [Node (Chest 1 4 []) []]])
-- [0,2,1,-1]
-- >>> allRoute (Node (Chest 0 0 [1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 [1,3]) [Node (Chest 1 1 []) [],Node (Chest 4 3 [2]) [Node (Chest 3 2 []) []]]])
-- [0,1,-1,2,1,-1,4,3,-1]
allRoute :: Node -> [Int]
allRoute n =
    let
        (Node c nx) = n
        (Chest i _ _) = c
    in
      if (length nx == 0) then
          [i,-1]
      else
          i:(concat (map allRoute nx))

getRouteIndex :: [Node]->[Int]
getRouteIndex xs =
    map (\(Node (Chest i k c) ns) -> i) xs

-- root node -> route list -> used key list -> answer route 
-- |
-- >>> allRouteWalk (Node (Chest 0 0 [1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 [1,3]) [Node (Chest 1 1 []) [],Node (Chest 4 3 [2]) [Node (Chest 3 2 []) []]]]) [] [] [] [] [] [] [] (Node (Chest 0 0 [1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 [1,3]) [Node (Chest 1 1 []) [],Node (Chest 4 3 [2]) [Node (Chest 3 2 []) []]]]) [] 0 4
-- [3,4,2,1,2,0]
-- >>> allRouteWalk (Node (Chest 0 0 [1,1,1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 []) [],Node (Chest 3 1 []) []]) [] [] [] [] [] [] [] (Node (Chest 0 0 [1,1,1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 []) [],Node (Chest 3 1 []) []]) [] 0 3
-- aaaaaaaaaa
allRouteWalk :: Node -> [Node] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Node -> [Node]->Int-> Int -> [Int]
allRouteWalk (Node c []) route have used walked bkHave bkUsed bkWalked bkRoot bkRoute fIndex goal=
    if ((length $ nub walked) == goal) then
        let (Chest i _ _) = c
        in i:walked;
    else
        if ((length route) == 0) then
              let (Node c ns) = bkRoot
              in
                if ((length ns)==0) then
                    []
                else
                    allRouteWalk (Node c (drop 1 ns)) (bkRoute) (bkHave) (bkUsed) (bkWalked) bkHave bkUsed bkWalked bkRoot bkRoute 0 goal
        else
            let
                (Node newC nx) = (head route)
                newNw = drop 1 nx
                (Chest i _ _) = c
            in
              allRouteWalk (Node newC newNw) (drop 1 route) (have) (used) (i:walked) bkHave bkUsed bkWalked bkRoot bkRoute 0 goal
allRouteWalk root route have used walked  bkHave bkUsed bkWalked bkRoot bkRoute fIndex goal=
    let
        (Node c nx) = root
        (Chest i k getKey) = c
        (Node nextC nextNx) = nx!!fIndex
        (Chest nextChestI nextChestK nextChestC) = nextC
        use = nextChestK
    in
      if (checkKey (getKey++have) used use) then
         allRouteWalk (nx!!fIndex) (root:route) (nextChestC++have) (use:used) (i:walked) have used walked root (root:route) 0 goal
      else
          if ((fIndex+1) < (length nx)) then
             allRouteWalk root route have used walked  bkHave bkUsed bkWalked bkRoot bkRoute (fIndex+1) goal
          else
              let (Node c ns) = bkRoot
              in
                if ((length ns)==0) then
                    []
                else
                    allRouteWalk (Node c (drop 1 ns)) (bkRoute) (bkHave) (bkUsed) (bkWalked) bkHave bkUsed bkWalked bkRoot bkRoute 0 goal

checkKey :: [Int] -> [Int] -> Int -> Bool
checkKey have used need = 
    let countHave = length $ filter (==need) have
        countUsed= length $ filter (==need) used
    in countUsed < countHave
-- |
-- >>> makeGraph [Chest 0 0 [5], Chest 1 4 [], Chest 2 5 [4]]
-- Node (Chest 0 0 [5]) [Node (Chest 2 5 [4]) [Node (Chest 1 4 []) []]]
-- >>> makeGraph [Chest 0 0 [1], Chest 1 1 [], Chest 2 1 [1,3], Chest 3 2 [], Chest 4 3 [2]]
-- Node (Chest 0 0 [1]) [Node (Chest 1 1 []) [],Node (Chest 2 1 [1,3]) [Node (Chest 1 1 []) [],Node (Chest 4 3 [2]) [Node (Chest 3 2 []) []]]]
-- >>> makeGraph [Chest 0 0 [1,1,1], Chest 1 1 [], Chest 2 1 [], Chest 3 1 []]
-- oooooooooo
makeGraph :: [Chest] -> Node
makeGraph xs =
    let
        c1 = head xs 
        n1 = graph c1 (drop 1 xs) []
    in
      n1             

graph :: Chest -> [Chest] -> [Int] -> Node 
graph x xs finded =
    let
        --矢印の元になる宝
        (Chest ti to tc) = x
        --参照がループしないようにする
        nfinded = ti:finded
        -- それで開けられるの一覧
        ns = concat $ nub $map (\k -> findChest k xs nfinded) tc
        n = (Node x (map (\nx->graph nx xs nfinded) ns))
    in
      n

-- |
-- >>> findChest 3 [Chest 1 5 [], Chest 2 3 [], Chest 3 9 [6,5,4], Chest 4 3 [9,8]] []
-- [Chest 2 3 [],Chest 4 3 [9,8]]
-- >>> findChest 5 [Chest 0 5 []] [0]
-- []
findChest :: Int -> [Chest] -> [Int] -> [Chest]
findChest k xs finded =
    filter (\(Chest i o c) -> o==k && (all (/=i) finded)) xs



