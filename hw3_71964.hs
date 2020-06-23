import Data.List
main :: IO ()
main = do
    
    print (containsWord t1 "acf")
    print (containsWord t1 "be")
    print (genWords t1)
    print (allContain [t1, t2, ])

data BTree = Empty | Node Char BTree BTree
                deriving Eq

t1 :: BTree                                       --      a
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)    --     / \
                        (Node 'd' Empty Empty))   --    c   b
              (Node 'b' Empty                     --   / \   \
                        (Node 'e' Empty Empty))   --  f   d   e


t2 :: BTree Char                               --     a
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty) --    / \
                        Empty)                 --   c   b
    (Node 'b' Empty Empty)                     --  /
                                               -- d

-- 1

containsWord :: BTree -> String -> Bool
containsWord Empty [] = True
containsWord (Node x Empty Empty) [y] 
                            |x == y    = True
                            |otherwise = False

containsWord (Node _ lTree rTree) [] = False
containsWord (Node x lTree rTree) (y:ys)
                            |x /= y         = False
                            |lTree == Empty = containsWord rTree ys
                            |rTree == Empty = containsWord lTree ys
                            |otherwise      = containsWord lTree ys || containsWord rTree ys 

-- 2

genWordsFromRoot :: BTree -> [String]
genWordsFromRoot root = helper root []
            where helper (Node x lTree rTree) temp
                                |lTree == Empty && rTree == Empty = [(temp ++ [x])]
                                |lTree == Empty = helper rTree (temp ++ [x])
                                |rTree == Empty = helper lTree (temp ++ [x])
                                |otherwise = helper rTree (temp ++ [x]) ++ helper lTree (temp ++ [x])

genWords :: BTree -> [String]
genWords Empty = []
genWords (Node x Empty Empty) = [[x]]
genWords (Node _ Empty rTree) = nub $ genWordsFromRoot rTree ++ genWords rTree
genWords (Node _ lTree Empty) = nub $ genWordsFromRoot lTree ++ genWords lTree
genWords root@(Node _ lTree rTree) = nub $ genWordsFromRoot root ++ genWordsFromRoot lTree ++ 
                                     genWordsFromRoot rTree ++ genWords lTree ++ genWords rTree

--1

allContain :: [BTree] -> [String]
allContain [] = []
allContain (x:[]) = genWords x
allContain (x:xs) = [word | word <- (genWords x), word `elem` (allContain xs)] -- elem proverqva dali v lista se sudurja dumata