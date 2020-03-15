main::IO()
main=do
    print (myGCD 24 22)
    print (countDigits 217)
    print(sumDigitsRec 1223)
    print (sumDigitsIter 43)
myGCD a b
    | a>b =myGCD (a-b) b
    | a<b =myGCD a (b - a)
    | otherwise =a    

countDigits a=
    if a<9 then 1
    else 1+ countDigits(a/10)

sumDigitsRec a=
    if a<10 then a
    else (a `mod` 10) + sumDigitsRec (a `div` 10)

sumDigitsIter a =
    helper 0 a

helper:: Int->Int->Int
helper sum a=
    if a == 0 then sum 
    else helper (sum+a`mod`10) (a`div`10)-- zashto trqbva da se slagat skobi

reverseNumber:: Int->Int
reverseNumber n=
    helper n 0

helper1 first second =
    if first==0 then second
    else (helper (first`div`10) (second*10+first`mod`10))