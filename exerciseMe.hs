main::IO()
main = do
    print(isEven 3)
    print(selectEven[1,2,3])
    print((\x y z -> x y z) (\x y -> x*x+y-1) 2 3 )
    print(sumOfDigits 123)
    print( isInteresting 411 )
    print (numbersInRange 1 100)
sumOfDigits :: Int -> Int
sumOfDigits n = (helper1 n 0)

helper1::Int->Int->Int
helper1 number result
    |number == 0 = result
    |otherwise   = helper1 (number`div`10) (result+(number`mod`10))

isInteresting:: Int->Bool 
isInteresting a =
    if (a`mod` (sumOfDigits a))== 0 then True else False

numbersInRange:: Int->Int->[Int]
numbersInRange a b = filter (\x -> containsSix x && has1reminderByDeviding4 x) [a..b]

containsSix::Int->Bool
containsSix a
    | a== 0       = False
    | a`mod`10==6 = True
    | otherwise   = containsSix(a`div`10)

has1reminderByDeviding4::Int->Bool
has1reminderByDeviding4 a=
    if a`mod` 4 ==1 then True else False


selectEvenMap::[Int]->[Int]
selectEvenMap xs= filter isEven xs

selectEven :: [Int]->[Int]
selectEven [] = []
selectEven (x:xs)
    |isEven x  = x:selectEven xs
    |otherwise = selectEven xs

isEven:: Int->Bool
isEven a
    |a `mod` 2==0 = True
    |otherwise = False