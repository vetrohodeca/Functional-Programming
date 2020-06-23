main :: IO()

main = do
    print (isSymetric t3)
    print (isSymetric t4)
    print (isSymetric t5)
{- 
rotate 3 ['a','b','c','d','e','f','g','h'] → "defghabc"
rotate (-2) ['a','b','c','d','e','f','g','h'] → "ghabcdef"
-}

getAt::Int -> [a] -> a
getAt index xs = 
    if index == 0
        then (head xs)
        else (getAt (index - 1) (tail xs))


helper:: Int -> [a] -> Int -> [a]
helper n xs repeat  
    | (n == repeat)     = [] 
    | (n < (length xs)) = (getAt n xs):(helper (n + 1) xs repeat)
    | otherwise         = (getAt (n `mod` (length xs)) xs):(helper ((n `mod` (length xs)) + 1) xs repeat)

rotate::Int -> [a] -> [a]
rotate n xs 
        |(n == 0) = xs
        |(n > 0) = (getAt n xs) : (helper ((n + 1) `mod` (length xs)) xs (n `mod` (length xs))) 
        |otherwise = (getAt (length xs -((abs n) `mod` (length xs))) xs) : (helper ((length xs) - (((abs n) - 1) `mod` (length xs))) xs ((length xs) - ((abs n) `mod` (length xs))))


data BTree = Empty | Node Int BTree BTree

t3 :: BTree
t3 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

t4 :: BTree
t4 = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 2 Empty (Node 3 Empty Empty))

t5 :: BTree
t5 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) Empty)) (Node 2 (Node 7 Empty  (Node 5 Empty Empty)) (Node 3 Empty Empty)) 

eQual::BTree->BTree->Bool
eQual Empty Empty = True
eQual _ Empty = False
eQual Empty _ = False
eQual (Node first Empty Empty) (Node second Empty Empty)   = (first == second)
eQual (Node first Empty rTree1) (Node second Empty rTree2) = False
eQual (Node first lTree1 Empty) (Node second Empty rTree2) = ((first == second) && (eQual lTree1 rTree2))
eQual (Node first Empty rTree1) (Node second lTree2 Empty) = ((first == second) && (eQual rTree1 lTree2))
eQual (Node first lTree1 Empty) (Node second lTree2 Empty) = False
eQual (Node first lTree1 rTree1) (Node second lTree2 rTree2) = (first == second) && (eQual rTree1 lTree2) && (eQual rTree1 lTree2)


isSymmetric :: BTree -> Bool
isSymmetric Empty = True
isSymetric (Node x Empty Empty) = True
isSymetric (Node x Empty rTree) = False
isSymetric (Node x lTree Empty) = False
isSymetric (Node x lTree rTree) = (eQual lTree rTree)