-- Made while trabelling from Dubai to Atlanta.

--equalVector a:as b:bs = if a \= b then False
--                                  else equal as bs
--equalVector [] [] = True


sub a b = add a (scalarMult b (-1))
add a b | valid a b = zipWith (+) a b
        | otherwise = [] -- throw an exception over here. Badly required.
dot a b | valid a b = zipWith (*) a b
        | otherwise = []
cross a b | (length b == 3)&&(length a == 3) = [(a !! 1)*(b !! 2)-(a !! 2)*(b !! 1),(a !! 2)*(b !! 0)-(a !! 0)*(b !! 2),(a !! 0)*(b !! 1)-(a !! 1)*(b !! 0) ]
          | otherwise = [] -- Terminating
-- only applicable in 3 dimensions. Don't care about n=7
valid a b = (length a == length b)
unitVector::(Floating a)=>[a]->[a]
unitVector a = scalarMult a (1/(magnitude a))
scalarMult::(Floating a)=>[a]->a->[a]
scalarMult (a:as) (b) = map (*b) (a:as)
factorial::(Integral a)=> a -> a
factorial a = if a >0 then foldr (\x y -> x*y) 1 [1..a] else 1
magnitude a = sqrt (foldr (\x y -> (x**2) + y ) 0 a)
triangle = [ (foldr (\x y -> x + y) 0 [1..x]) | x<-[1..]]
parallelTo a b | unitVector a == unitVector b = True
               | otherwise = False
errorMessage :: IO()
errorMessage  = putStrLn "Error. Please check for inconsistent code."
averageSlope a b c d= (b-a)/(d-c)

dis t = (-2)*8.4*t + (3*8.4*t*t)
