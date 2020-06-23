import Data.List
main::IO()
main = do
    print (isInteresting 11)
    print (sumInInterval 60 62)
    print (isProgression [1,2,4])
    print (factorial 6)
    print (sin1 10 1,57)
    print (dominates double tripple [1,2,3])
    print (hardestSubject db)
--1 zad
isInteresting:: Int-> Bool
isInteresting a =
    if(a`mod` sumOfDigits a ==0) then True else False
    
sumOfDigits:: Int-> Int
sumOfDigits a
    |a==0      = 0
    |otherwise = a`mod`10+sumOfDigits(a`div`10)

--2 zad
sumInInterval:: Int->Int->Int
sumInInterval a b = sum[k|k<-[a..b],k`mod`4==1,consistSix k]

consistSix::Int->Bool
consistSix a
    |a==0         = False
    |a`mod`10 ==6 = True
    |otherwise    = consistSix(a`div` 10)

-- 3 zad

isProgression :: [Int] -> Bool -- pri rabota sus spisuci neshtata ne vlizat navutre
isProgression []           = False
isProgression [f]          = True
isProgression [f, s]       = True
isProgression  (f:s:t:xs)  = (((f-s) == (s-t)) && isProgression(s:t:xs))

progressions::[[Int]]->[[Int]]
progressions xs= [s | s <- xs, isProgression s]

-- 4 zad
factorial:: Int->Int
factorial a
    |a==0      = 0
    |a==1      = 1
    |otherwise = a * (factorial (a - 1))

helper::Int->Double->Int->Double->Double
helper n x i result      
    |n == 0    = result
    |otherwise = helper (n - 1) x (i + 1) (result + ((-1)^ i * x ^ (2 * i + 1)) /fromIntegral((factorial 2 * i + 1)))  -- i to ni komutira znaka kato stepenen pokazatel na -1

sin1 :: Int->Double->Double
sin1 n x= helper n x 0 0 

-- 5 zad
dominates:: (Int->Int) ->(Int->Int) ->[Int] ->Bool
dominates f g []     = True
-- dominates f g [x]    =  (f x) >= abs (g x)   
dominates f g (x:xs) = abs (f x) >= abs (g x) && dominates f g xs

double::Int->Int
double x= 2*x    

tripple::Int->Int
tripple a=3*a

-- 6 zad
type Student = String  --име на ученикtype Subject = String  --име на предметtype Note = Double 
type Subject = String
type Note = Double
type Record = (Student, Subject, Note) -- структура за студент
db :: [Record] -- danni spisuk ot studenti
db = [("Ivan", "Matematika", 5.50) , ("Ivan", "Fizika", 6.00) , ("Ivan", "Himiq", 4.00),
      ("Georgi", "Matematika", 5.00) , ("Georgi", "Fizika" , 5.00) , ("Georgi", "Himiq", 5.00),
      ("Petar", "Matematika", 5.00) , ("Petar", "Fizika", 5.50) , ("Petar", "Himiq", 4.00)]



hardestSubject :: [Record] ->Subject
hardestSubject db =fst lowestAverageNote
    where 
        subjects = nub [subject | ( _, subject, _)<- db]
        notes =[(subject, [note | (_, subjectName, note) <- db, subjectName == subject]) | subject <- subjects]
        average ns= sum ns / (fromIntegral (length  ns))
        averageNotes= [(subject, average notes) | (subject, notes ) <- notes]
        lowest n1@(_, av1) n2@(_, av2)= if av1 < av2  then n1 else n2
        lowestAverageNote= foldl1 lowest averageNotes
            