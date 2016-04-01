

--List of normal constants (SI)
_g_ = 9.8 --m s-2
_G_ = 6.67408e-11 --m3 kg-1 s-2
_ke_ = 8.9875517873681764e9  --N m2 C2
_magneticConstant_= 4*pi*(1e-7) -- N A−2
_electric constant_ =  8.854187817e-13  --F m−1
_planckConstant_ = 6.62606876e-34
_elementaryCharge_ = 1.602176462e-19


--Useful Physics Function
gravitationalForce m1 m2 r = _G_*m1*m2/(r*r)
coulombForce q1 q2 r = _ke_*q1*q2/(r*r)


-- Functions that make boring clerical work easier
merge :: [a] -> [a] -> [a]
merge (a:as) (b:bs) = [a,b] ++ (merge as bs)
merge [] []=[]
average :: (Fractional a, Foldable t) => t a -> a
average a = sum a / fromIntegral (length a)
standardDeviation :: Floating a => [a] -> a
standardDeviation a = sqrt ((foldr (+) 0 (map (\x->(x-(average a))^2) a))/fromIntegral (length a))
standardError :: Floating a => [a] -> a
standardError a = (standardDeviation a)/ sqrt (fromIntegral (length a))
factorial a = if a >0 then foldr (\x y -> x*y) 1 [1..a] else 1

-- Functions related to Riemann Sum of a function.

lowerPoints :: Ord a => [a] -> [a]
lowerPoints a    | length a == 0 = [] --Should never happen
                 | length a == 1 = a --Should never happen
                 | length a == 2 = [min (a!!0) (a!!1)]
                 | length a > 2  = [min (a!!0) (a!!1)] ++ lowerPoints (tail a)
upperPoints :: Ord a => [a] -> [a]
upperPoints a    | length a == 0 = [] --Should never happen
                 | length a == 1 = a --Should never happen
                 | length a == 2 = [max (a!!0) (a!!1)]
                 | length a > 2  = [max (a!!0) (a!!1)] ++ upperPoints (tail a)

lowerRiemannSum::(Enum a, Fractional a, Ord a)=> (a -> a) -> a -> a -> a -> a
lowerRiemannSum func startPoint endPoint noOfRectangle = foldr (+) 0 (map (*((endPoint-startPoint)/noOfRectangle)) (lowerPoints (map (func) [startPoint,startPoint + (startPoint+endPoint)/noOfRectangle .. endPoint])))
upperRiemannSum::(Enum a, Fractional a, Ord a)=> (a -> a) -> a -> a -> a -> a
upperRiemannSum func startPoint endPoint noOfRectangle = foldr (+) 0 (map (*((endPoint-startPoint)/noOfRectangle)) (upperPoints (map (func) [startPoint,startPoint + (startPoint+endPoint)/noOfRectangle .. endPoint])))
rightRiemannSum::(Enum a, Fractional a, Ord a)=> (a -> a) -> a -> a -> a -> a
rightRiemannSum func startPoint endPoint noOfRectangle = foldr (+) 0 (map (*((endPoint-startPoint)/noOfRectangle)) (tail (map (func) [startPoint,startPoint + (startPoint+endPoint)/noOfRectangle .. endPoint])))
leftRiemannSum::(Enum a, Fractional a, Ord a)=> (a -> a) -> a -> a -> a -> a
leftRiemannSum  func startPoint endPoint noOfRectangle = foldr (+) 0 (map (*((endPoint-startPoint)/noOfRectangle)) (init (map (func) [startPoint,startPoint + (startPoint+endPoint)/noOfRectangle .. endPoint])))

-- Useful Sequences
triangleSequence = [ (foldr (\x y -> x + y) 0 [1..x]) | x<-[1..]]
squareSequence = [x^2 | x<-[1..]]
pentagonalSequence = p_gonalSequence 5
p_gonalSequence p | p<3 = [] --there is no shape with less than three side
                  | otherwise = [ (p-2)*x*(x-1)/2 + x | x<-[1..] ]
railroad n = merge [1..(n `div` 2)] [n,n-1..(n `div` 2)+1]
--catalanNumbers x = combinationFormula 2x x


-- Functions that are related to vectors. Makes life easier
sub:: Floating c => [c] -> [c] -> [c]
sub a b = add a (scalarMult b (-1))
add:: Floating c => [c] -> [c] -> [c]
add a b | valid a b = zipWith (+) a b
        | otherwise = [] -- throw an exception over here. Badly required.
--dot a b | valid a b = sum (zipWith (*) a b)
--        | otherwise = []
cross:: Num t => [t] -> [t] -> [t]
cross a b | (length b == 3)&&(length a == 3) = [(a !! 1)*(b !! 2)-(a !! 2)*(b !! 1),(a !! 2)*(b !! 0)-(a !! 0)*(b !! 2),(a !! 0)*(b !! 1)-(a !! 1)*(b !! 0) ]
          | otherwise = [] -- Terminating
valid::(Foldable t, Foldable t1) => t a -> t1 a1 -> Bool
valid a b = (length a == length b)
unitVector::(Floating a)=>[a]->[a]
unitVector a = scalarMult a (1/(magnitude a))
scalarMult::(Floating a)=>[a]->a->[a]
scalarMult (a:as) (b) = map (*b) (a:as)
magnitude::(Floating a)=>[a]->a
magnitude a = sqrt (foldr (\x y -> (x**2) + y ) 0 a)


-- need to learn how to use the Data.matrix features.



-- Random space for on-the-fly functions.
