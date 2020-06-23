import Data.List
main :: IO()
main = do

    print (perimeter(Rectangle 4 5))

data Shape = Rectangle Double Double | Circle Double | Triangle Double Double Double

perimeter:: Shape -> Double
perimeter (Circle r)       = 3.14*r*2
perimeter (Rectangle a b)  = 2*(a+b)
perimeter (Triangle a b c) = a+b+c

