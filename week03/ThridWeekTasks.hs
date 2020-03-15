main::IO()
main=do
    print(pow 5 1)
    print(isPrime 7)
    print(isAscending 45)
    print(countOccurences 666 0)
    print(isPerfectNumber 8128)
    print( sumPrimeDivisors 10)
pow::Double->Int->Double
pow x n  =
    (helper x n 1)
  
helper::Double->Int->Double->Double
helper x n res
    | n==0 = 1
    | n==1 = res*x
    | otherwise = (helper x  (n-1)  (res*x))

isPrime::Int->Bool
isPrime n =(helper1 2 n)  

helper1::Int->Int->Bool
helper1 counter n 
    | n < 2 = False
    | counter == n = True
    | (n `mod` counter) == 0 = False
    | otherwise = (helper1 (counter+1)  n)

isAscending::Int->Bool
isAscending n= helper2 n (n`mod`10)

helper2::Int->Int->Bool
helper2 number digit
    | number == 0 = True
    | number`mod`10>digit = False
    | otherwise= (helper2 (number`div`10) (number`mod`10))

countOccurences::Int->Int->Int
countOccurences n digit=
   (helper3 n 0 digit )

helper3::Int->Int->Int->Int
helper3 number counter digit
    | number == 0 = counter
    | number`mod`10 == digit =(helper3 (number`div`10) (counter+1) digit)
    | otherwise= (helper3 (number`div`10) counter digit)

isPerfectNumber::Int->Bool
isPerfectNumber n=
    n== (helper4 2 n 1)

helper4::Int->Int->Int->Int
helper4 devider number sum
    | number < 2 =1
    | devider == number = sum
    | (number `mod` devider) == 0 = (helper4 (devider+1) number (sum+devider))
    | otherwise = (helper4 (devider+1)  number sum)

sumPrimeDivisors::Int->Int
sumPrimeDivisors n =
    (helper5 2 n 0)

helper5::Int->Int->Int->Int
helper5 devider number sum
    | number < 2 =1
    | devider == number = sum
    | (number `mod` devider) == 0&&(isPrime devider) = (helper5 (devider+1) number (sum+devider))
    | otherwise = (helper5 (devider+1)  number sum)