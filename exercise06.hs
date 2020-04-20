main ::IO ()
main= do

    multiplyALLBy ::[Int] -> Int -> [Int]
    multiplyALLBy xs k= [x * k |x <- xs]

    multiplyALLBy1 ::[Int] -> Int -> [Int]
    multiplyALLBy xs k = map (*k) xs

    filterSmallerThan :: [Int] -> Int -> [Imt]
    filterSmallerThan xs n = [x } x <-xs , x >= n]

     filterSmallerThan :: [Int] -> Int -> [Imt]
      filterSmallerThan xs