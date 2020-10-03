main::IO()
main=do
    print (5 &&& 6)
    print (f [5,2] )
    print(mySum 10 0 0)
    print (f2 5)
    print ((twice f2) 5)
    print  [["1","Hi"],["21","Hi","Everybody"]]

f2:: Int->Int
f2 a = a*a
twice :: (Int -> Int) -> (Int -> Int)
twice f2 = (f2 . f2)
--iterativen proces
mySum::Int->Int->Int->Int
mySum maxNumber counter result
    |counter>maxNumber = result
    |otherwise         = mySum maxNumber (counter+1) (result+counter)

f :: [Int] -> [Int]
f [] = []
f [_] = []
f (x:xs) = (x*(length xs)):(f xs)

--definirane na operator min
(&&&):: Int->Int->Int
x &&& y
    | x > y       = y
    | otherwise   = x