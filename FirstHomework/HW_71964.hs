main :: IO()
main= do
    print((findSum 0 2 10))
    print(isSquare 3)
    print(isSpecial 353 2) 
findSum::Int->Int->Int->Int
findSum a b n  =
   ((helper1 a b (n-1) b)+
   (helper1 a b (n-2) b)+
   (helper1 a b (n-3) b))
   


helper1:: Int->Int->Int->Int->Int -- namira sumata na chlena na radeicata
helper1 a b counter member
    | counter == 0 = (a+member)
    | otherwise    = (helper1 a b (counter-1) (member+ (2^counter)*b))

isSquare::Int->Bool
isSquare n =
    (helper2 n 1)

helper2::Int->Int->Bool
helper2 num currentNum
    | currentNum*currentNum == num = True
    | currentNum > (num`div`2)     = False
    | otherwise                    = (helper2 num (currentNum+1)) 



isPrime::Int->Bool
isPrime n =(helper3 2 n)  

helper3::Int->Int->Bool
helper3 counter n 
    | n < 2                  = False
    | counter == n           = True
    | (n `mod` counter) == 0 = False
    | otherwise              = (helper3 (counter+1)  n)

isSpecial:: Int->Int->Bool
isSpecial n k
    | ((countDigit n) -k )<0  = False -- ako imame po- golqmo k ot broq na cifrite-> false
    | ((countDigit n) -k )==0 = (isPrime (helper4 n k )) -- ako imame ravno k -> pravim chislata i proverqvame dali sa prosti
    | otherwise               = (isPrime (helper4 n k ))&& (isSpecial (n`div`10) k) -- pravim logikata za chisloto bez poslednata cifra

helper4::Int->Int->Int
helper4 number k 
    | k==1      = (number`mod`10)
    | otherwise = (((helper4 (number`div`10) (k-1))*10)+(number`mod`10))--vrushtame rekursivno 1 chislo nazad

countDigit::Int->Int
countDigit n
    |n<10      =1
    |otherwise = 1+(countDigit (n`div` 10))