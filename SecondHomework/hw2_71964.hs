main::IO()
main = do
    print (helper 2 1)
    print (generate 0.1 5)
    print (listSquares 1 10)
    print (sumOfDevidersHelper 9 1)
    print (listSquares 1 30)
    print (checkInCircle (3, 0) 3 (0,0) )
    print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])   --([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])

-- 1 zad 

generate::Double-> Int-> [Double]
generate p n
    |n==1      = [1]
    |otherwise =   (generate p (n-1)) ++ [(helper (fromIntegral n) p)]
helper:: Double-> Double-> Double
helper a p     
    |a == 1    =1.0
    |otherwise = ((helper (a - 1) p)+ (1.0 / (a**p)))
 

 --2 zad

listSquares :: Int-> Int-> [(Int , Int)]
listSquares a b =
    [(x , (sumOfDevidersHelper x 1)) | x<-(filter isSquare [a..b])] -- pravim naredena dvoika ot x i sbora na delitelite(smqta go funkciqta) i go prilagame za vseki element s napisanoto otdqsno na |

isSquare:: Int -> Bool  
isSquare a= (isSquareHelper a 1) 

isSquareHelper:: Int -> Int->Bool
isSquareHelper number counter  
    |counter*counter == number = True
    |counter*counter > number  = False
    |otherwise                 = (isSquareHelper number (counter+1))

sumOfDevidersHelper:: Int->Int->Int
sumOfDevidersHelper number counter 
    |counter > number          = 0
    |number `mod` counter == 0 = counter*counter+ (sumOfDevidersHelper number (counter + 1))
    |otherwise                 = (sumOfDevidersHelper number (counter+1))

-- 3 zad

type Point = (Double, Double)
splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints center radius points= ((filter (checkInCircle center radius) points), (filter (\ x -> (not (checkInCircle center radius x))) points))

checkInCircle:: Point -> Double -> Point -> Bool
checkInCircle center radius point =
    if((((fst point) - (fst center))^2 + ((snd point) - (snd center))^2) <=radius^2) then True else False


-- 4 zad

type Person = (Int, String, String)
type Account = (Int, Int, Double)
ps :: [Person] 
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
as :: [Account] 
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2),(5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]
