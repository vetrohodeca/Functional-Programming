main::IO()
main=do
    print (5 &&& 6)

--definirane na operator min
(&&&):: Int->Int->Int
x &&& y
    | x > y       = y
    | otherwise   = x