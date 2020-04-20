main::IO()
main=do
    let numbers = [4,8,15,16,23,42]  
    print (count numbers)
    print (mySum numbers)
    print (isInList 8 numbers)
    print (simpleNumberBetweenAandB 2 7)
    print (removeFirstElement 2 [1,2,2,3])
    print (removeAllElements 2 [1,2,2,3])
    print (incrementAllBy [1,2,3,4] 5)
count :: [a] -> Int
count []     = 0
count (x:xs) = 1 + count xs

mySum :: [Double] -> Double
mySum []     = 0
mySum (x:xs) = x + mySum xs

isInList:: Eq t => t -> [t] -> Bool
isInList _ []            = False
isInList number (x:xs) = number == x || isInList number xs

simpleNumberBetweenAandB start end
    | start > end =[]
    | otherwise = [k | k <- [start..end], isPrime k]-- kato cikul?

isPrime:: Int->Bool
isPrime a =[1,a]==[b | b <- [1..a], mod a b ==0]-- kakvo pravi tova

removeFirstElement:: Int->[Int]->[Int]
removeFirstElement _ []     = []
removeFirstElement n (x:xs) =
    if x==n then xs else x : (removeFirstElement n xs)

removeAllElements:: Int->[Int]->[Int]
removeAllElements _ []     = []
removeAllElements n (x:xs) =
    if x==n then (removeAllElements n xs) else x:(removeAllElements n xs)

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] _     = []    
incrementAllBy xs n = [x + n |x <- xs]

--x:xs represent a list which x is the first element (head) and xs is the rest of the list (tail).
--подаване на списък [type] 
--_подаване на аргумент когато е  без значение
-- Eq t => за дефиниция на генеричен парамерър на фунцкия
-- как се дефинира списък 
-- как се записва списък