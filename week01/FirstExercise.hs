main::IO()
main = do
  print (myMin 5 6)
  print (inside 2 5 3)
  print(calcAverage 2 3)
  print(fibRec 5)
  print(fibIter 5 )
myMin a b=
    if a<b then a else b

    -- koga e nujno da pishem deklaraciq
inside a b number = -- kak se naricha tozi element
  if number>=a&&number<=b 
  then True 
  else False

calcAverage a b=
    (a*a+b*b)/2

fibRec n -- guard operator i nqma ravno
    | n==0 = 1
    | n==1 = 1
    | otherwise =fibRec(n-1)+fibRec(n-2)

helper index previous current=
    if index<=1 then current
    else helper(index-1 )current (previous+current)

fibIter n=
    helper n 1 1
--VAJNO:
--trqbva da ima -> mejdu parametrite

--Vuprosi 
--kolko intervala trqbva da postavqme za razdelqne na blokovete
--zaduljitelno li e pri guard operatora da polzvame deklaraciq 