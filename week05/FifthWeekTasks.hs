main::IO()
main = do
    print (mIdentity 5)
    print( difference help1 2 8 )

mIdentity:: Eq t => t -> t
mIdentity x=x    

myCompose:: (a -> b) -> (c -> a) -> (c -> b)
myCompose f g = \ x -> f (g x)--kakvo pravi \

myCompose1 f g = f.g

myNegate::(a->Bool)->(a->Bool)
myNegate p = \ x -> (not (p x))

myNegate1 p x= not (p x)

difference:: (Double->Double)->Double->Double->Double
difference f a b = (f b)- (f a)

help1:: Double -> Double
help1 a= a+5